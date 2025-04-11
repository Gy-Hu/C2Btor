#!/usr/bin/env python3

"""A K2 to C translator

Warning: still not complete, but might serve as an example of using the Kratos
Python API
"""

from kratos import *
import sys
import re
import argparse

_c_names = {
    'true' : '1',
    'false' : '0',
    'main' : 'main__',
    'return' : 'return__',
    'break' : 'break__',
    'continue' : 'continue__',
    'fabs' : 'fabs__',
    'ceil' : 'ceil__',
    }
_c_id = 1

def get_c_name(name):
    if name in _c_names:
        return _c_names[name]
    base = re.sub('[^a-zA-Z0-9_]', '_', name)
    cname = base
    global _c_id
    while cname in _c_names:
        cname = base + '_' + _c_id
        _c_id += 1
    _c_names[name] = cname
    return cname
        

_c_enum_id = {}
def to_c_type(tp):
    if isinstance(tp, IntType):
        return 'int'
    elif isinstance(tp, VoidType):
        return 'void'
    elif isinstance(tp, BoolType):
        return '_Bool'
    elif isinstance(tp, RealType):
        return 'double'
    elif isinstance(tp, BVType):
        bvtp = {
            (8, False) : 'uint8_t',
            (8, True) : 'int8_t',
            (16, False) : 'uint16_t',
            (16, True) : 'int16_t',
            (32, False) : 'uint32_t',
            (32, True) : 'int32_t',
            (64, False) : 'uint64_t',
            (64, True) : 'int64_t',
            }
        s = bvtp.get((tp.bits, tp.is_signed))
        if s is not None:
            return s
        else:
            # find the closest match...
            for w in sorted(bvtp):
                if tp.bits <= w[0] and tp.is_signed == w[1]:
                    return bvtp[w]
            raise NotImplementedError(str(tp))
    elif isinstance(tp, FPType):
        fptp = [
            ((5, 10), '_Float16'),
            ((8, 23), 'float'),
            ((11, 52), 'double'),
            ]
        for k, v in fptp:
            if tp.exponent <= k[0] and tp.mantissa <= k[1]:
                return v
        raise NotImplementedError(str(tp))
    elif isinstance(tp, EnumType):
        global _c_enum_id
        i = _c_enum_id.get(tp)
        if i is None:
            raise NotImplementedError(str(tp))
        return 'Enum__%s' % i
    elif isinstance(tp, MapType):
        return 'Map_%s_%s' % (to_c_type(tp.index).replace(' ', '_'),
                              to_c_type(tp.element).replace(' ', '_'))
    else:
        raise NotImplementedError(str(tp))

class CExprPrinter(ExprVisitor):
    def __init__(self, out, error_name=None, error_as_assert=False):
        self.out = out
        self.opmap = {
            OpExpr.ADD : '+',
            OpExpr.SUB : '-',
            OpExpr.MUL : '*',
            OpExpr.NEG : '-',
            OpExpr.DIV : '/',
            OpExpr.REM : '%',
            OpExpr.LSHIFT : '<<',
            OpExpr.RSHIFT : '>>', 
            OpExpr.BITAND : '&',
            OpExpr.BITOR : '|',
            OpExpr.BITXOR : '^',
            OpExpr.BITNOT : '~',
            OpExpr.EQUAL : '==',
            OpExpr.AND : '&&',
            OpExpr.OR : '||',
            OpExpr.NOT : '!',
            OpExpr.LE : '<=',
            OpExpr.LT : '<',
            OpExpr.GE : '>=',
            OpExpr.GT : '>',
            }
        self.indent = 0
        self.error_name = error_name
        self.error_as_assert = error_as_assert

    def is_error(self, expr):
        if not expr.annotations:
            return False
        for (k, v) in expr.annotations:
            if k == 'error' and \
               (self.error_name is None or v == self.error_name):
                return True
        return False

    def visit_stmt(self, expr):
        if not isinstance(expr, SeqExpr):
            self.out.write('  ' * self.indent)
        self.visit(expr)
        if not isinstance(expr, SeqExpr):
            self.out.write(';')

    def visit_decl(self, expr, is_stmt=True, is_output=False):
        assert isinstance(expr, VarExpr)
        name = get_c_name(expr.var)
        if is_stmt:
            self.out.write('  ' * self.indent)
        qual = '' if not is_output else '*'
        self.out.write('%s %s%s' % (to_c_type(expr.type), qual, name))
        if is_stmt:
            self.out.write(';')

    def visit_VarExpr(self, expr):
        name = get_c_name(expr.var)
        self.out.write(name)

    def visit_ConstExpr(self, expr):
        if isinstance(expr.type, EnumType):
            val = '%s__%s' % (to_c_type(expr.type), get_c_name(expr.value))
        elif not isinstance(expr.type, FunctionType):
            self.out.write('(%s)' % to_c_type(expr.type))
            try:
                float(expr.value)
                val = expr.value
            except ValueError:
                val = get_c_name(expr.value)
        else:
            val = get_c_name(expr.value)
        self.out.write(val)

    def visit_OpExpr(self, expr):
        if expr.op == OpExpr.MAPGET:
            self.out.write('*%s_AT(' % to_c_type(expr.args[0].type))
            self.visit(expr.args[0])
            self.out.write(', ')
            self.visit(expr.args[1])
            self.out.write(')')
        elif expr.op == OpExpr.MAPSET:
            raise NotImplementedError(str(expr))
        else:
            self.out.write('(')
            if len(expr.args) == 2:
                self.visit(expr.args[0])
            self.out.write(' %s ' % self.opmap[expr.op])
            self.visit(expr.args[-1])
            self.out.write(')')

    def visit_CallExpr(self, expr):
        if expr.ret:
            self.visit(expr.ret[0])
            self.out.write(' = ')
        self.visit(expr.func)
        self.out.write('(')
        sep = ''            
        for a in expr.args:
            self.out.write(sep)
            sep = ', '
            self.visit(a)
        for r in expr.ret[1:]:
            self.out.write(sep)
            sep = ', '
            self.out.write('&')
            self.visit(r)
            self.out.write('')
        self.out.write(')')

    def visit_AssignExpr(self, expr):
        if isinstance(expr.lhs.type, MapType):
            if isinstance(expr.rhs, OpExpr) and expr.rhs.op == OpExpr.MAPSET \
               and expr.lhs.equals(expr.rhs.args[0]):
                self.out.write('*%s_AT(' % to_c_type(expr.lhs.type))
                self.visit(expr.lhs)
                self.out.write(', ')
                self.visit(expr.rhs.args[1])
                self.out.write(') = ')
                self.visit(expr.rhs.args[2])
            else:
                raise NotImplementedError(str(expr))
        else:
            self.visit(expr.lhs)
            self.out.write(' = ')
            self.visit(expr.rhs)

    def visit_AssumeExpr(self, expr):
        if isinstance(expr.cond, ConstExpr) and expr.cond.type == BoolType() \
           and expr.cond.value == 'true':
            pass
        else:
            self.out.write('__VERIFIER_assume(')
            self.visit(expr.cond)
            self.out.write(')')

    def visit_TypeCastExpr(self, expr):
        self.out.write('(%s)' % to_c_type(expr.type))
        self.visit(expr.expr)

    def visit_SeqExpr(self, expr):
        n = len(expr.args)-1
        for i, e in enumerate(expr.args):
            self.visit_stmt(e)
            if i < n:
                self.out.write('\n')

    def visit_JumpExpr(self, expr):
        n = len(expr.targets)
        tab = '  ' * self.indent
        if n == 1:
            self.out.write('goto ')
            self.out.write(get_c_name(expr.targets[0].name))
        elif n == 2:
            self.out.write('if (__VERIFIER_nondet_bool()) {\n')
            self.out.write(tab + '  goto %s;' %
                           get_c_name(expr.targets[0].name))
            self.out.write('\n' + tab + '} else {\n')
            self.out.write(tab + '  goto %s;' %
                           get_c_name(expr.targets[1].name))
            self.out.write('\n' + tab + '}')
        else:
            self.out.write('switch (__VERIFIER_nondet_int()) {\n')
            for i, e in enumerate(expr.targets):
                if i < n:
                    self.out.write(tab + 'case %d:\n' % i)
                else:
                    self.out.write(tab + 'default:\n')
                self.out.write(tab + '  goto ')
                self.out.write(get_c_name(e.name))
                self.out.write(';\n')
                self.out.write('  ' * self.indent)
                self.out.write(tab + '  break;\n')
            self.out.write(tab + '}')

    def visit_LabelExpr(self, expr):
        self.out.write(get_c_name(expr.name))
        self.out.write(':')
        if self.is_error(expr):
            if self.error_as_assert:
                self.out.write(' assert(0)')
            else:
                self.out.write(' __VERIFIER_error()')

    def visit_HavocExpr(self, expr):
        if isinstance(expr.type, MapType):
            name = get_c_name(expr.lval.var)
            self.out.write('%s.sz = 0;\n' % name)
            self.out.write('%s.data = NULL;\n' % name)
        else:
            self.visit(expr.lval)
            self.out.write(' = ')
            self.out.write(self.get_nondet_func(expr.lval.type))

    def visit_CondJumpExpr(self, expr):
        tab = '  ' * self.indent
        self.out.write('if (')
        self.visit(expr.cond)
        self.out.write(') {\n')
        self.out.write(tab + '  goto %s;' % get_c_name(expr.target.name))
        self.out.write('\n' + tab + '}')

    def get_nondet_func(self, tp):
        if isinstance(tp, (IntType, BVType)):
            return '__VERIFIER_nondet_int()'
        elif isinstance(tp, BoolType):
            return '__VERIFIER_nondet_bool()'
        elif isinstance(tp, (RealType, FPType)):
            return '__VERIFIER_nondet_double()'
        elif isinstance(tp, EnumType):
            return '(%s)__VERIFIER_nondet_int()' % to_c_type(tp)
        else:
            raise NotImplementedError(tp)

# end of class CExprPrinter


class UFCollector(ExprVisitor):
    def __init__(self, program):
        super().__init__()
        self.fns = {f.name for f in program.functions}
        self.ret = {}

    def visit_CallExpr(self, expr):
        if isinstance(expr.func, ConstExpr):
            f = expr.func.value
            if f not in self.fns:
                self.ret[f] = expr.func
        for a in expr.args:
            self.visit(a)
        for r in expr.ret:
            self.visit(r)

# end of class UFCollector


def print_function(out, f, error_name, error_as_assert, prototype_only=False):
    printer = CExprPrinter(out, error_name, error_as_assert)
    printer.indent = 1
    out.write('\n')
    if f.retvars:
        out.write(to_c_type(f.retvars[0].type))
    else:
        out.write('void')
    out.write(' %s(' % get_c_name(f.name))
    n = len(f.params)-1
    sep = ''
    for v in f.params:
        out.write(sep)
        sep = ', '
        printer.visit_decl(v, False)
    for v in f.retvars[1:]:
        out.write(sep)
        sep = ', '
        printer.visit_decl(VarExpr(v.type, 'out_' + v.var + '__'), False, True)
    out.write(')')
    if prototype_only:
        out.write(';\n')
    else:
        out.write('\n{\n')
        for v in f.retvars:
            printer.visit_decl(v)
            out.write('\n')
        for v in f.locals:
            printer.visit_decl(v)
            out.write('\n')

        for vl in (f.params, f.retvars, f.locals):
            for v in vl:
                if isinstance(v.type, MapType):
                    name = get_c_name(v.var)
                    out.write('  %s.sz = 0;\n  %s.data = NULL;\n' %
                              (name, name))
        
        printer.visit_stmt(f.body)
        out.write('\n')
        for v in f.retvars[1:]:
            out.write('\n  *%s = %s;\n' % (get_c_name('out_' + v.var + '__'),
                                           get_c_name(v.var)))
            out.write('\n')
        if f.retvars:
            out.write('\n  return %s;\n' % get_c_name(f.retvars[0].var))
        out.write('}\n')


def print_function_prototype(out, f):
    print_function(out, f, None, False, True)
    
    
def print_program(opts, out, p):
    class TypeInfoCollector(ExprVisitor):
        def __init__(self):
            self.maps = set()
            self.enums = set()

        def visit_type(self, tp):
            if isinstance(tp, MapType):
                self.maps.add(tp)
                self.visit_type(tp.index)
                self.visit_type(tp.element)
            elif isinstance(tp, EnumType):
                self.enums.add(tp)
            elif isinstance(tp, FunctionType):
                for a in tp.args:
                    self.visit_type(a)
                for r in tp.ret:
                    self.visit_type(r)
            
        def generic_visit(self, expr):
            self.visit_type(expr.type)
            super(TypeInfoCollector, self).generic_visit(expr)

    # end of class TypeInfoCollector

    if not opts.seahorn:
        out.write('#include <stdlib.h>\n')
        out.write('#include <inttypes.h>\n\n')
        out.write('extern void __VERIFIER_error();\n')
        out.write('extern void __VERIFIER_assume(_Bool cond);\n')
    else:
        out.write('#include "seahorn/seahorn.h"\n\n')
        out.write('typedef unsigned long size_t;\n')
    out.write('extern int __VERIFIER_nondet_int();\n')
    out.write('extern double __VERIFIER_nondet_double();\n')
    out.write('extern _Bool __VERIFIER_nondet_bool();\n')
    
    mc = TypeInfoCollector()
    for v in p.globals:
        mc.visit(v)
    if p.init:
        mc.visit(p.init)
    for f in p.functions:
        for v in f.locals:
            mc.visit(v)
        for r in f.retvars:
            mc.visit(r)
        for v in f.params:
            mc.visit(v)
        mc.visit(f.body)

    for m in sorted(mc.maps, key=to_c_type):
        if not isinstance(m.index, (IntType, BVType)):
            raise NotImplementedError(str(m))
        name = to_c_type(m)
        elem = to_c_type(m.element)
        out.write('\ntypedef struct {\n')
        out.write('  unsigned int sz;\n')
        out.write('  %s *data;\n' % elem)
        out.write('} %s;\n\n' % name)
        out.write('%s *%s_AT(%s m, unsigned int idx)\n{\n' %
                  (elem, name, name))
        out.write('  while (m.sz <= idx) {\n')
        out.write('    m.sz *= 2;\n')
        out.write('    m.data = realloc(m.data, m.sz);\n')
        out.write('  }\n')
        out.write('  return &(m.data[idx]);\n}\n\n')
    for e in sorted(mc.enums, key=str):
        _c_enum_id[e] = len(_c_enum_id) + 1
        name = to_c_type(e)
        out.write('\ntypedef enum {\n')
        sep = ''
        for v in e.values:
            val = '%s__%s' % (name, get_c_name(v))
            out.write(sep)
            out.write('   %s' % val)
            sep = ',\n'
        out.write('\n} %s;\n\n' % name)

    ufs = UFCollector(p)
    for f in p.functions:
        print_function_prototype(out, f)
        ufs.visit(f.body)

    if ufs.ret:
        out.write('\n/* uninterpreted functions */')
        for key in sorted(ufs.ret):
            c = ufs.ret[key]
            f = Function(c.value, [VarExpr(t, 'p%d' % i)
                                   for (i, t) in enumerate(c.type.args)],
                         [VarExpr(t, 'r%d' % i)
                          for (i, t) in enumerate(c.type.ret)], [], None)
            print_function_prototype(out, f)
        out.write('\n')

    printer = CExprPrinter(out)
    for v in p.globals:
        printer.visit_decl(v)
        out.write('\n')

    for f in p.functions:
        print_function(out, f, opts.error_name, opts.error_as_assert)
    
    out.write('\nint main()\n{\n')
    printer.indent = 1
    # declare args and returns of the entry point
    if p.entrypoint:
        f = p.get_function(p.entrypoint)
        for v in f.params:
            printer.visit_decl(v)
            out.write('\n')
        for v in f.retvars:
            printer.visit_decl(v)
            out.write('\n')
    # first, make sure that globals are uninitialized
    for v in p.globals:
        if isinstance(v.type, MapType):
            name = get_c_name(v.var)
            out.write('  %s.sz = 0;\n  %s.data = NULL;\n' % (name, name))
        else:
            printer.visit_stmt(HavocExpr(v))
        out.write('\n')
    if p.init:
        printer.visit_stmt(p.init)
        out.write('\n')
    if p.entrypoint:
        args = []
        f = p.get_function(p.entrypoint)
        for v in f.retvars[1:]:
            args.append('&%s' % get_c_name(v.var))
        out.write('  %s(%s);\n' % (get_c_name(p.entrypoint), ', '.join(args)))
    out.write('  return 0;\n')
    out.write('}\n')


def getopts():
    p = argparse.ArgumentParser()
    p.add_argument('--seahorn', action='store_true',
                   help='use seahorn syntax')
    p.add_argument('--error-name')
    p.add_argument('--error-as-assert', action='store_true')
    return p.parse_args()


def main():
    opts = getopts()
    prog = parse_program(sys.stdin)
    print_program(opts, sys.stdout, prog)


if __name__ == '__main__':
    main()

