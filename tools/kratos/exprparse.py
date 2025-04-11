import sys, copy
try:
    import ply.yacc as yacc
except ImportError:
    import pycparser.ply.yacc as yacc
from kratos.exprlex import ExprLexer
from kratos.expr import *
from collections import OrderedDict as odict


class _Parser(object):
    def __init__(self, start_sym=None, debug=False):
        self.lex = ExprLexer()
        self.lex.build()
        self.tokens = self.lex.tokens
        kwds = {
            'write_tables' : False,
            'errorlog' : yacc.NullLogger(),
            'debug' : False,
            }
        if debug:
            kwds['debug'] = True
            del kwds['errorlog']
        if start_sym is not None:
            kwds['start'] = start_sym

        ops = "add sub mul neg div rem lshift rshift " \
            "bitand bitor bitxor bitnot and or not " \
            "eq le lt ge gt floor isfinite isinf isnan mapget mapset ite".split()
        self.opmap = dict((n, eval("OpExpr." + n.upper()))
                          for n in ops if n != "eq")
        self.opmap["eq"] = OpExpr.EQUAL

        self.global_bindings = {
            'true' : ConstExpr(True),
            'false' : ConstExpr(False),
            }
        self.local_bindings = {}
        self.type_bindings = {}
            
        self.parser = yacc.yacc(module=self, tabmodule="exprtab", **kwds)

        
    def p_start(self, p):
        "start : program"
        p[0] = p[1]


    def p_program(self, p):
        "program : decllist"
        glbls = None
        init = None
        entrypoint = None
        functions = []
        for (kind, o) in p[1]:
            if kind == 'globals':
                if glbls is not None:
                    raise Exception("multiple globals declarations")
                glbls = o
            elif kind == 'function':
                functions.append(o)
            elif kind == 'init':
                if init is not None:
                    raise Exception("multiple init declarations")
                init = o
            elif kind == 'entrypoint':
                if entrypoint is not None:
                    raise Exception("multiple entrypoint declarations")
                entrypoint = o
            elif kind in ('type', 'define'):
                pass
            else:
                raise Exception("unknown declaration kind: %s" % kind)
        p[0] = Program(glbls or [], init, entrypoint, functions)


    def p_empty(self, p):
        "empty : "
        pass


    def p_decllist(self, p):
        """decllist : decllist decl
                    | empty"""
        if len(p) == 3:
            p[0] = p[1]
            p[0].append(p[2])
        else:
            p[0] = []


    def p_decl(self, p):
        """decl : globals_decl
                | function_decl
                | init_decl
                | entrypoint_decl
                | type_decl
                | define_decl"""
        p[0] = p[1]


    def p_globals_decl(self, p):
        "globals_decl : '(' GLOBALS var_list ')'"
        p[0] = ('globals', p[3])
        for v in p[3]:
            self.global_bindings[v.var] = v

    def p_function_decl(self, p):
        """function_decl : basic_function_decl
                         | annotated_function_decl"""
        p[0] = p[1]


    def p_basic_function_decl(self, p):
        "basic_function_decl : '(' start_function_decl sym '(' function_var_list ')' '(' RETURN function_var_list ')' '(' LOCALS function_var_list ')' expr ')'"
        f = Function(p[3], p[5], p[9], p[13], p[15])
        p[0] = ('function', f)
        self.global_bindings[f.name] = f.get_const()
        self.local_bindings = {}


    def p_annotated_function_decl(self, p):
        "annotated_function_decl : '(' '!' basic_function_decl annotations ')'"
        p[0] = p[3]
        p[0][1].annotations = p[4]


    def p_function_var_list(self, p):
        "function_var_list : var_list"
        p[0] = p[1]
        for v in p[1]:
            self.local_bindings[v.var] = v


    def p_start_function_decl(self, p):
        "start_function_decl : FUNCTION"
        p[0] = None
        self.local_bindings = {}
        

    def p_init_decl(self, p):
        "init_decl : '(' INIT expr ')'"
        e = p[3]
        if not isinstance(e, AssumeExpr):
            e = AssumeExpr(e)
        p[0] = ('init', e)


    def p_entrypoint_decl(self, p):
        "entrypoint_decl : '(' ENTRY sym ')'"
        p[0] = ('entrypoint', p[3])


    def p_type_decl(self, p):
        "type_decl : '(' TYPE sym type ')'"
        name = p[3]
        self.type_bindings[name] = p[4]
        p[0] = ('type', None)


    def p_define_decl(self, p):
        "define_decl : '(' DEFINE sym expr ')'"
        name = p[3]
        self.global_bindings[name] = p[4]
        p[0] = ('define', None)
        

    def p_sym(self, p):
        "sym : SYMBOL"
        p[0] = p[1]


    def p_expr(self, p):
        """expr : binding_expr
                | var_expr
                | const_expr
                | op_expr
                | call_expr
                | assign_expr
                | assume_expr
                | typecast_expr
                | cast_expr
                | bitcast_expr
                | seq_expr
                | jump_expr
                | label_expr
                | havoc_expr
                | condjump_expr
                | annotated_expr"""
        p[0] = p[1]


    def p_binding_expr(self, p):
        "binding_expr : sym"
        b = self.local_bindings.get(p[1], self.global_bindings.get(p[1]))
        if b is None:
            raise KeyError("binding not found: %s" % p[1])
        p[0] = copy.copy(b)


    def p_var_expr(self, p):
        "var_expr : '(' VAR sym type ')'"
        p[0] = VarExpr(p[4], p[3])


    def p_const_expr(self, p):
        """const_expr : '(' CONST sym type ')'
                      | '(' CONST '(' const_map ')' type ')'"""
        if len(p) == 6:
            p[0] = ConstExpr(p[4], p[3])
        else:
            p[0] = ConstExpr(p[6], p[4])

    def p_const_map(self, p):
        """const_map : const_map_entry
                     | const_map_entry const_map"""
        k, v = p[1]
        if len(p) == 2:
            p[0] = odict()
        else:
            p[0] = p[2]
        p[0][k] = v

    def p_const_map_entry(self, p):
        """const_map_entry : '(' '(' ')' DOT const_map_value ')'
                           | '(' sym DOT const_map_value ')'"""
        if len(p) == 7:
            p[0] = (None, p[5])
        else:
            p[0] = (p[2], p[4])

    def p_const_map_value(self, p):
        """const_map_value : sym
                           | '(' const_map ')'"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = p[2]


    def p_op_expr(self, p):
        """op_expr : '(' OP op_tag expr_list ')'
                   | '(' SYMBOL expr_list ')'"""
        if p[2] == "op":
            p[0] = OpExpr(p[3], *p[4])
        else:
            p[0] = OpExpr(self.opmap[p[2]], *p[3])


    def p_op_tag(self, p):
        "op_tag : SYMBOL"
        p[0] = self.opmap[p[1]]


    def p_expr_list(self, p):
        """expr_list : expr_list expr
                     | empty"""
        if len(p) == 3:
            p[0] = p[1]
            p[0].append(p[2])
        else:
            p[0] = []


    def p_call_expr(self, p):
        "call_expr : '(' CALL expr expr_list ')'"
        n = len(p[3].type.args)
        p[0] = CallExpr(p[3], p[4][:n], p[4][n:])


    def p_idx(self, p):
        "idx : SYMBOL"
        p[0] = int(p[1])


    def p_assign_expr(self, p):
        "assign_expr : '(' ASSIGN expr expr ')'"
        p[0] = AssignExpr(p[3], p[4])


    def p_assume_expr(self, p):
        "assume_expr : '(' ASSUME expr ')'"
        p[0] = AssumeExpr(p[3])


    def p_typecast_expr(self, p):
        "typecast_expr : '(' TYPECAST type expr idx ')'"
        p[0] = TypeCastExpr(p[3], p[4], p[5])


    def p_cast_expr(self, p):
        "cast_expr : '(' CAST type expr ')'"
        p[0] = TypeCastExpr(p[3], p[4], 0)


    def p_bitcast_expr(self, p):
        "bitcast_expr : '(' BITCAST type expr ')'"
        p[0] = TypeCastExpr(p[3], p[4], 1)


    def p_seq_expr(self, p):
        "seq_expr : '(' SEQ expr_list ')'"
        p[0] = SeqExpr(*p[3])


    def p_jump_expr(self, p):
        "jump_expr : '(' JUMP expr_list ')'"
        p[0] = JumpExpr(*p[3])


    def p_label_expr(self, p):
        "label_expr : '(' LABEL sym ')'"
        p[0] = LabelExpr(p[3])


    def p_havoc_expr(self, p):
        "havoc_expr : '(' HAVOC expr ')'"
        p[0] = HavocExpr(p[3])


    def p_condjump_expr(self, p):
        "condjump_expr : '(' CONDJUMP expr expr ')'"
        p[0] = CondJumpExpr(p[3], p[4])


    def p_annotated_expr(self, p):
        "annotated_expr : '(' '!' expr annotations ')'"
        p[0] = p[3]
        p[0].annotations = p[4]


    def p_annotated_var_expr(self, p):
        """annotated_var_expr : var_expr
                              | '(' '!' var_expr annotations ')'"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = p[3]
            p[0].annotations = p[4]


    def p_annotations(self, p):
        """annotations : KEYWORD sym
                       | annotations KEYWORD sym"""
        if len(p) == 3:
            p[0] = [(p[1][1:], p[2])]
        else:
            p[0] = p[1]
            p[0].append((p[2][1:], p[3]))


    def p_var_list(self, p):
        """var_list : var_list annotated_var_expr
                    | empty"""
        if len(p) == 3:
            p[0] = p[1]
            p[0].append(p[2])
        else:
            p[0] = []


    def p_type(self, p):
        """type : binding_type
                | void_type
                | bool_type
                | int_type
                | real_type
                | bv_type
                | fp_type
                | map_type
                | sym_type
                | fun_type
                | enum_type"""
        p[0] = p[1]


    def p_binding_type(self, p):
        "binding_type : sym"
        b = self.type_bindings.get(p[1])
        if b is None:
            raise KeyError("type binding not found: %s" % p[1])
        p[0] = copy.copy(b)
        

    def p_void_type(self, p):
        "void_type : VOID"
        p[0] = VoidType()


    def p_bool_type(self, p):
        "bool_type : BOOL"
        p[0] = BoolType()


    def p_int_type(self, p):
        "int_type : INT"
        p[0] = IntType()


    def p_real_type(self, p):
        "real_type : REAL"
        p[0] = RealType()


    def p_bv_type(self, p):
        """bv_type : sbv_type
                   | ubv_type
                   | genbv_type"""
        p[0] = p[1]

    def p_sbv_type(self, p):
        "sbv_type : '(' SBV idx ')'"
        p[0] = BVType(p[3], 1)

    def p_ubv_type(self, p):
        "ubv_type : '(' UBV idx ')'"
        p[0] = BVType(p[3], 0)

    def p_genbv_type(self, p):
        "genbv_type : '(' BV idx idx ')'"
        p[0] = BVType(p[3], p[4])


    def p_fp_type(self, p):
        "fp_type : '(' FP idx idx ')'"
        p[0] = FPType(p[3], p[4])


    def p_map_type(self, p):
        "map_type : '(' MAP type type ')'"
        p[0] = MapType(p[3], p[4])


    def p_sym_type(self, p):
        "sym_type : '(' SYM sym ')'"
        p[0] = SymbolType(p[3])


    def p_fun_type(self, p):
        "fun_type : '(' FUN '(' type_list ')' '(' type_list ')' ')'"
        p[0] = FunctionType(p[4], p[7])


    def p_enum_type(self, p):
        "enum_type : '(' ENUM sym_list  ')'"
        p[3].sort()
        p[0] = EnumType(p[3])
        

    def p_type_list(self, p):
        """type_list : type_list type
                     | empty"""
        if len(p) == 3:
            p[0] = p[1]
            p[0].append(p[2])
        else:
            p[0] = []


    def p_sym_list(self, p):
        """sym_list : sym_list sym
                    | empty"""
        if len(p) == 3:
            p[0] = p[1]
            p[0].append(p[2])
        else:
            p[0] = []


    def p_error(self, p):
        raise Exception("syntax error at `%s' (line: %s)" %
                        (p.value, p.lexer.lineno))

    def parse(self, data):
        return self.parser.parse(input=data, lexer=self.lex)

# end of class _Parser


class ExprParser(_Parser):
    def __init__(self):
        super(ExprParser, self).__init__('expr')

# end of class ExprParser


class ProgramParser(_Parser):
    def __init__(self, debug=False):
        super(ProgramParser, self).__init__(debug=debug)

# end of class ProgramParser


class ExecutionPathParser(object):
    def __init__(self):
        class TypeDeclParser(_Parser):
            def __init__(self):
                super(TypeDeclParser, self).__init__('type_decl')
                
        self.parser = ExprParser()
        self.tparser = TypeDeclParser()
        self.parser.type_bindings = self.tparser.type_bindings

    def parse(self, data):
        pths = [ExecutionPath()]
        levels = [0]
        for line in data.splitlines():
            s = line.strip()
            if not s or s.startswith(';'):
                continue

            try:
                stmt = self.parser.parse(s)
                if isinstance(stmt, CallExpr) \
                   and isinstance(stmt.func, ConstExpr):
                    self.parser.global_bindings[stmt.func.value] = stmt.func
            except Exception as e:
                stmt = self.tparser.parse(s)
                continue

            lvl = len(line) - len(line.lstrip())

            if lvl == 0 and isinstance(stmt, AssignExpr) \
                   and isinstance(stmt.lhs, VarExpr):
                self.parser.global_bindings[stmt.lhs.var] = stmt.lhs
                
            if lvl == levels[-1]:
                pths[-1].append(ExecutionStep(stmt))
            elif lvl > levels[-1]:
                if pths[-1] and not pths[-1][-1].is_path \
                   and isinstance(pths[-1][-1].stmt, CallExpr):
                    s = pths[-1][-1]
                    pths[-1].pop()
                    pths.append(ExecutionPath())
                    pths[-1].append(s)
                else:
                    pths.append(ExecutionPath())
                levels.append(lvl)
                pths[-1].append(ExecutionStep(stmt))
            else:
                if len(pths) <= 1:
                    raise Exception("bad indentation in execution path")
                s = ExecutionStep(pths[-1])
                pths.pop()
                levels.pop()
                while levels and lvl > levels[-1]:
                    levels.pop()
                    pths.pop()
                if not levels:
                    #if lvl != levels[-1]:
                    raise Exception("bad indentation in execution path")
                pths[-1].append(s)
                pths[-1].append(ExecutionStep(stmt))

        while len(pths) > 1:
            p = pths[-1]
            pths.pop()
            pths[-1].append(ExecutionStep(p))
        return pths[0]
            
# end of class ExecutionPathParser


if __name__ == '__main__':
    parser = ProgramParser(True)
    prog = parser.parse(sys.stdin.read())
    sys.stdout.write('PARSED:\n')
    prog.write(sys.stdout)
