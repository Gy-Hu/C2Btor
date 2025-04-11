#!/usr/bin/env python3

"""A K2 to Boogie (https://boogie-docs.readthedocs.io) translator

Warning: still not complete, but might serve as an example of using the Kratos
Python API
"""

from kratos import *
import sys
import re
import argparse

_boogie_names = {
    'true' : 'true',
    'false' : 'false',
    }
_boogie_id = 1

def get_boogie_name(name):
    if name in _boogie_names:
        return _boogie_names[name]
    base = re.sub("[^a-zA-Z0-9_.~^$#'`?]", '_', name)
    cname = base
    global _boogie_id
    while cname in _boogie_names:
        cname = base + '_' + _boogie_id
        _boogie_id += 1
    _boogie_names[name] = cname
    return cname
        

def to_boogie_type(tp):
    if isinstance(tp, IntType):
        return 'int'
    elif isinstance(tp, BoolType):
        return 'bool'
    elif isinstance(tp, RealType):
        return 'real'
    elif isinstance(tp, BVType):
        return 'bv%d' % tp.bits
    elif isinstance(tp, MapType):
        return '[%s]%s' % (to_boogie_type(tp.index),
                           to_boogie_type(tp.element))
    else:
        raise NotImplementedError(tp)

builtin_ops = {
    OpExpr.ADD : '+',
    OpExpr.SUB : '-',
    OpExpr.MUL : '*',
    OpExpr.NEG : '-',
    OpExpr.DIV : 'div',
    OpExpr.REM : 'mod',
    OpExpr.EQUAL : '==',
    OpExpr.AND : '&&',
    OpExpr.OR : '||',
    OpExpr.NOT : '!',
    OpExpr.LE : '<=',
    OpExpr.LT : '<',
    OpExpr.GE : '>=',
    OpExpr.GT : '>',
    }

def get_bv_op(op, tp):
    names = {
        OpExpr.ADD : 'bvadd',
        OpExpr.SUB : 'bvsub',
        OpExpr.MUL : 'bvmul',
        OpExpr.NEG : 'bvneg',
        (OpExpr.DIV, False) : 'bvudiv',
        (OpExpr.DIV, True) : 'bvsdiv',
        (OpExpr.REM, False) : 'bvurem',
        (OpExpr.REM, True) : 'bvsrem',
        OpExpr.LSHIFT : 'bvshl',
        (OpExpr.RSHIFT, False) : 'bvlshr',
        (OpExpr.RSHIFT, True) : 'bvashr',
        OpExpr.BITAND : 'bvand',
        OpExpr.BITOR : 'bvor',
        OpExpr.BITXOR : 'bvxor',
        OpExpr.BITNOT : 'bvnot',
        (OpExpr.LE, False) : 'bvule',
        (OpExpr.LE, True) : 'bvsle',
        (OpExpr.LT, False) : 'bvult',
        (OpExpr.LT, True) : 'bvslt',
        (OpExpr.GE, False) : 'bvuge',
        (OpExpr.GE, True) : 'bvsge',
        (OpExpr.GT, False) : 'bvugt',
        (OpExpr.GT, True) : 'bvsgt',
        }
    ret = names.get(op)
    if ret is None:
        ret = names[(op, tp.is_signed)]
    assert ret is not None
    return ret


def collect_uninterpreted_functions(program):
    defuns = set(f.name for f in program.functions)
    ret = {}

    class FunCollector(ExprVisitor):
        def visit_CallExpr(self, e):
            self.generic_visit(e)
            if isinstance(e.func, ConstExpr) and e.func.value not in defuns:
                ret[e.func.value] = (e.func.value, e.func.type, None)

        def visit_OpExpr(self, e):
            self.generic_visit(e)
            tp = e.args[0].type
            if isinstance(tp, BVType) and e.op != OpExpr.EQUAL:
                name = 'bv%d%s%s' % (tp.bits, 's' if tp.is_signed else 'u',
                                     OpExpr.opstr[e.op])
                opname = get_bv_op(e.op, tp)
                optype = FunctionType([a.type for a in e.args], [e.type])
                ret[name] = (name, optype, ':bvbuiltin "%s"' % opname)
            elif e.op not in builtin_ops:
                name = 'UF.%s' % OpExpr.opstr[e.op]
                ret[name] = (name, FunctionType([a.type for a in e.args],
                                                [e.type]), None)

    # end of class FunCollector

    fc = FunCollector()
    if program.init:
        fc.visit(program.init)

    for f in program.functions:
        fc.visit(f.body)

    return ret


def remove_fun_arg_assignment(program):
    class AssignChecker(ExprVisitor):
        def __init__(self):
            self.assigned = set()

        def visit_AssignExpr(self, e):
            if isinstance(e.lhs, VarExpr):
                self.assigned.add(e.lhs.var)

    # end of class AssignChecker

    class AssignRewriter(IdentityVisitor):
        def __init__(self, assigned):
            self.assigned = assigned

        def visit_VarExpr(self, e):
            if e.var in self.assigned:
                return VarExpr(e.type, self.assigned[e.var])
            else:
                return e

    # end of class AssignRewriter
    
    for fn in program.functions:
        c = AssignChecker()
        c.visit(fn.body)
        vp = VarProvider(program.globals, fn.locals, fn.params, fn.retvars)
        assigned = {}
        stmts = []
        for p in fn.params:
            if p.var in c.assigned:
                v = vp.mkvar(p.type, p.var)
                assigned[p.var] = v.var
                stmts.append(AssignExpr(v, p))
                fn.locals.append(v)
        if assigned:
            rw = AssignRewriter(assigned)
            stmts.append(rw.visit(fn.body))
            fn.body = SeqExpr(*stmts)

    return program
    

class BoogieExprPrinter(ExprVisitor):
    def __init__(self, out, error_label_fun, funmap, vp):
        self.out = out
        self.error_label_fun = error_label_fun
        self.funmap = funmap
        self.opmap = builtin_ops
        self.indent = 0
        self.vp = vp

    def visit_stmt(self, expr):
        if not isinstance(expr, SeqExpr):
            self.out.write('  ' * self.indent)
        self.visit(expr)
        if not isinstance(expr, (SeqExpr, LabelExpr)):
            self.out.write(';')

    def visit_decl(self, expr, is_stmt=True):
        assert isinstance(expr, VarExpr)
        name = get_boogie_name(expr.var)
        if is_stmt:
            self.out.write('  ' * self.indent)
            self.out.write('var ')
        self.out.write('%s: %s' % (name, to_boogie_type(expr.type)))
        if is_stmt:
            self.out.write(';')

    def visit_VarExpr(self, expr):
        name = get_boogie_name(expr.var)
        self.out.write(name)

    def visit_ConstExpr(self, expr):
        if not isinstance(expr.type, FunctionType):
            #self.out.write('(%s)' % to_boogie_type(expr.type))
            try:
                float(expr.value)
                val = expr.value
                if isinstance(expr.type, RealType) and '.' not in val:
                    val += '.0'
                elif isinstance(expr.type, BVType):
                    val += 'bv%d' % expr.type.bits
            except ValueError:
                val = get_boogie_name(expr.value)
        else:
            val = get_boogie_name(expr.value)
        self.out.write(val)

    def try_eval(self, expr):
        tp = expr.args[0].type
        toeval = {
            OpExpr.BITAND: lambda a, b : a & b,
            OpExpr.BITOR: lambda a, b : a | b,
            OpExpr.BITXOR: lambda a, b : a ^ b,
            OpExpr.BITNOT: lambda a : ~a,
            OpExpr.LSHIFT: lambda a, b : a << b,
            OpExpr.RSHIFT: lambda a, b : a >> b,
            }
        def getargs():
            for a in expr.args:
                if isinstance(a, ConstExpr):
                    n = int(a.value)
                    if n >= 0:
                        yield a
                else:
                    c = self.try_eval(a)
                    if c is not None:
                        yield c
                    else:
                        raise Exception("not a non-negative integer: %s" %
                                        str(a))
            
        if isinstance(tp, IntType) and expr.op in toeval:
            try:
                args = getargs()
                res = toeval[expr.op](*[int(a.value) for a in args])
                c = ConstExpr(IntType(), str(res))
                return c
            except Exception as e:
                #sys.stderr.write('PROBLEM: %s\n' % str(e))
                return None

    def visit_OpExpr(self, expr):
        tp = expr.args[0].type
        special = None
        if expr.op == OpExpr.MAPGET:
            self.visit(expr.args[0])
            self.out.write('[')
            self.visit(expr.args[1])
            self.out.write(']')
            return
        if isinstance(tp, BVType) and expr.op != OpExpr.EQUAL:
            special = 'bv%d%s%s' % (tp.bits, 's' if tp.is_signed else 'u',
                                    OpExpr.opstr[expr.op])
        elif expr.op not in self.opmap:
            c = self.try_eval(expr)
            if c is not None:
                self.visit_ConstExpr(c)
                return
            special = 'UF.%s' % OpExpr.opstr[expr.op]

        if special is not None:
            self.out.write(special)
            sep = '('
            for a in expr.args:
                self.out.write(sep)
                sep = ','
                self.visit(a)
            self.out.write(')')
        else:
            try:
                op = self.opmap[expr.op]
            except KeyError:
                raise NotImplementedError(expr)
            if op == 'div' and isinstance(expr.args[0].type, RealType):
                op = '/' # special case
            self.out.write('(')
            if len(expr.args) == 2:
                self.visit(expr.args[0])
            self.out.write(' %s ' % op)
            self.visit(expr.args[-1])
            self.out.write(')')

    def visit_CallExpr(self, expr):
        if isinstance(expr.func, ConstExpr) \
               and expr.func.value not in self.funmap:
            self.out.write('call ')
        sep = ''
        for r in expr.ret:
            self.out.write(sep)
            sep = ', '
            self.visit(r)
        if expr.ret:
            self.out.write(' := ')
        self.visit(expr.func)
        self.out.write('(')
        sep = ''
        for a in expr.args:
            self.out.write(sep)
            sep = ', '
            self.visit(a)
        self.out.write(')')

    def visit_AssignExpr(self, expr):
        if isinstance(expr.rhs, OpExpr) and expr.rhs.op == OpExpr.MAPSET:
            if expr.lhs.equals(expr.rhs.args[0]):
                self.visit(expr.lhs)
                self.out.write('[')
                self.visit(expr.rhs.args[1])
                self.out.write(']')
                self.out.write(' := ')
                self.visit(expr.rhs.args[2])
            else:
                raise NotImplementedError(expr)
        else:
            self.visit(expr.lhs)
            self.out.write(' := ')
            self.visit(expr.rhs)

    def visit_AssumeExpr(self, expr):
        self.out.write('assume ')
        self.visit(expr.cond)

    def visit_TypeCastExpr(self, expr):
        if expr.type == expr.expr.type:
            self.visit(expr.expr)
        if isinstance(expr.type, BVType) and isinstance(expr.expr.type, BVType) \
           and expr.type.bits == expr.expr.type.bits:
            self.visit(expr.expr)
        elif isinstance(expr.type, BoolType) \
           and isinstance(expr.expr.type, (IntType, RealType, BVType)):
            self.out.write('(')
            self.visit(expr.expr)
            zero = '0'
            if isinstance(expr.expr.type, BVType):
                zero = '0bv%d' % expr.expr.type.bits
            elif isinstance(expr.expr.type, RealType):
                zero += '.0'
            self.out.write(' != %s)' % zero)
        elif isinstance(expr.type, IntType) \
             and isinstance(expr.expr.type, RealType):
            self.out.write("int(")
            self.visit(expr.expr)
            self.out.write(")")
        elif isinstance(expr.type, RealType) \
             and isinstance(expr.expr.type, IntType):
            self.out.write("real(")
            self.visit(expr.expr)
            self.out.write(")")
        elif isinstance(expr.type, IntType) \
             and isinstance(expr.expr.type, BoolType):
            self.out.write("(if ")
            self.visit(expr.expr)
            self.out.write(" then 1 else 0)")
        else:
            raise NotImplementedError(expr)

    def visit_SeqExpr(self, expr):
        n = len(expr.args)-1
        for i, e in enumerate(expr.args):
            self.visit_stmt(e)
            if i < n:
                self.out.write('\n')

    def visit_JumpExpr(self, expr):
        self.out.write('goto %s' % ', '.join(
            get_boogie_name(t.name) for t in expr.targets))

    def visit_LabelExpr(self, expr):
        self.out.write(get_boogie_name(expr.name))
        self.out.write(':')
        if self.error_label_fun(expr):
            self.out.write(' assert false;')

    def visit_HavocExpr(self, expr):
        self.out.write('havoc ')
        self.visit(expr.lval)

    def visit_CondJumpExpr(self, expr):
        lthen = LabelExpr(self.vp.mkname('$then'))
        lelse = LabelExpr(self.vp.mkname('$else'))
        self.visit(JumpExpr(lthen, lelse))
        self.out.write(';\n')
        self.visit_stmt(SeqExpr(
            lthen,
            AssumeExpr(expr.cond),
            JumpExpr(expr.target),
            lelse,
            AssumeExpr(OpExpr(OpExpr.NOT, expr.cond))
            ))

# end of class BoogieExprPrinter


def print_function(out, p, f, error_label_fun, funmap):
    is_entrypoint = f.name == p.entrypoint
    vp = VarProvider(p.globals, f.params, f.locals, f.retvars)
    printer = BoogieExprPrinter(out, error_label_fun, funmap, vp)
    printer.indent = 1
    out.write('\nprocedure%s %s(' % (' {:entrypoint}' if is_entrypoint else '',
                                     get_boogie_name(f.name)))
    n = len(f.params)-1
    sep = ''
    for v in f.params:
        out.write(sep)
        sep = ', '
        printer.visit_decl(v, False)
    out.write(')')
    if f.retvars:
        out.write(' returns (')
    sep = ''
    for v in f.retvars:
        out.write(sep)
        sep = ', '
        printer.visit_decl(v, False)
    if f.retvars:
        out.write(')')
    out.write('\n{\n')
    for v in f.locals:
        printer.visit_decl(v)
        out.write('\n')

    printer.visit_stmt(f.body)
    out.write('\n')
    out.write('}\n')
    
    
def print_program(out, p, error_label_fun):
    funmap = collect_uninterpreted_functions(p)

    for fname in sorted(funmap):
        out.write('function %s' % fname)
        _, tp, annot = funmap[fname]
        if annot is not None:
            out.write(' %s ' % annot)
        out.write('(%s) returns(%s);\n' %
                  (', '.join(to_boogie_type(a) for a in tp.args),
                   to_boogie_type(tp.ret[0])))

    printer = BoogieExprPrinter(out, error_label_fun, funmap,
                                VarProvider(p.globals))
    for v in p.globals:
        printer.visit_decl(v)
        out.write('\n')

    for f in p.functions:
        print_function(out, p, f, error_label_fun, funmap)


def main():
    p = argparse.ArgumentParser()
    p.add_argument('--error-label', action='append',
                   help='specify the error label (can occur multiple times)')
    p.add_argument('--error-label-prefix', 
                   help='specify the error label prefix')
    p.add_argument('--error-label-regex',
                   help='specify the error label(s) with a regular expression')
    opts = p.parse_args()
    prog = parse_program(sys.stdin)
    prog = remove_fun_arg_assignment(prog)
    errlbls = set(opts.error_label if opts.error_label else [])
    allerrlbls = not opts.error_label and not opts.error_label_regex \
        and not opts.error_label_prefix
    def error_label_fun(lbl):
        if not lbl.annotations:
            return False
        for (key, val) in lbl.annotations:
            if key == 'error':
                name = val
                break
        if allerrlbls:
            return True
        elif name in errlbls:
            return True
        elif opts.error_label_regex \
             and re.match(opts.error_label_regex, name):
            return True
        elif opts.error_label_prefix \
             and name.startswith(opts.error_label_prefix):
            return True
        return False
    print_program(sys.stdout, prog, error_label_fun)


if __name__ == '__main__':
    main()

