#!/usr/bin/env python

"""
Translates a C file to Kratos2 IR.

Requires pycparser: https://github.com/eliben/pycparser
"""

from __future__ import print_function
import os, sys
from pycparser import c_parser, c_ast, c_generator, c_lexer
import re
from io import StringIO
import subprocess
import math
import itertools
from kratos import *
from .expr import *
from .rewrite import *
from collections import OrderedDict
from functools import reduce


class Unsupported(Exception):
    def __init__(self, msg):
        super(Exception, self).__init__("unsupported: " + str(msg))

# end of class Unsupported


class _tostr(object):
    def __init__(self, n, msg=None):
        self.n = n
        self.msg = msg

    def __str__(self):
        buf = StringIO()
        self.n.show(buf)
        g = c_generator.CGenerator()
        buf.write('\n')
        buf.write(g.visit(self.n))
        if self.msg:
            buf.write('\n' + self.msg)
        return buf.getvalue()

# end of class _tostr


class Runtime(object):
    def get_basic_type(self, tp):
        raise NotImplementedError

    def get_pointer_type(self, tp):
        return None

    def is_builtin(self, name):
        raise NotImplementedError

    def get_builtin_call(self, call, visitor):
        raise NotImplementedError

    def get_constant(self, c):
        raise NotImplementedError

    def get_type_promotion(self, a, b):
        raise NotImplementedError

    def unsupported(self, node):
        raise Exception("unsupported: %s" % _tostr(node))

    def setup_visitor(self, visitor):
        raise NotImplementedError

# end of class Runtime


class DefaultCRuntime(Runtime):
    kratos_inttype_re = re.compile(r'^_Kratos_u?int([0-9+])_t$')
    
    def __init__(self, opts):
        self.opts = opts
        self.assert_cnt = 1
        self.inttype_re = re.compile(r'^u?int([0-9]+)_t$')

        self.builtin_call_map = {}
        for attr in dir(self.opts):
            if attr.startswith('builtin_'):
                func = '_builtin_call_' + attr[8:]
                if hasattr(self, func):
                    key = getattr(self.opts, attr)
                    self.builtin_call_map[key] = getattr(self, func)

    def get_basic_type(self, tp):
        last = tp.names[-1]
        sz_16 = {
            'int' : 2,
            'long' : 2,
            'unsigned' : 2,
            'short' : 2,
            'char' : 1
        }
        sz_32 = {
            'int' : 4,
            'long' : 4,
            'unsigned' : 4,
            'short' : 2,
            'char' : 1
        }
        sz_64 = {
            'int' : 4,
            'long' : 8,
            'unsigned' : 4,
            'short' : 2,
            'char' : 1                
        }
        intsizes = {
            16: sz_16,
            32: sz_32,
            64: sz_64,
            True: sz_32, # backwards compatibility
        }
        floatsizes = {
            '_Float16' : (5, 10),
            'float' : (8, 23),
            'double' : (11, 52),
        }
        if last in intsizes[32]:
            if self.opts.bitvectors:
                is_signed = 'unsigned' not in tp.names
                if last == 'int' and len(tp.names) > 1 and \
                   tp.names[-2] == 'short':
                    last = 'short'
                sz = intsizes[self.opts.bitvectors][last]
                if last == 'long' and len(tp.names) > 1 and \
                   tp.names[-2] == 'long':
                    sz = 8
                return BVType(sz * 8, is_signed)
            else:
                return IntType()
        elif last in floatsizes:
            if self.opts.bitvectors and not self.opts.floats_as_reals:
                sizes = floatsizes[last]
                if tp.names == ["long", "double"]:
                    sizes = (15, 112)
                return FPType(*sizes)
            else:
                return RealType()
        elif last == '_Bool':
            return BoolType()
        elif last == 'void' and len(tp.names) == 1:
            return VoidType()
        m = self.inttype_re.match(last)
        if m is None and self.opts.custom_int_types:
            m = self.kratos_inttype_re.match(last)
            if m:
                last = last[8:] # strip the _Kratos_ prefix
        if m is not None:
            if self.opts.bitvectors:
                sz = m.group(1)
                is_signed = 'unsigned' not in tp.names \
                            and not last.startswith('u')
                return BVType(int(sz), is_signed)
            else:
                return IntType()
        return None

    def get_pointer_type(self, tp):
        return None

    def is_builtin(self, name):
        return name in self.builtin_call_map \
            and name not in self.opts.funs_blacklist

    def get_builtin_call(self, call, visitor):
        if type(call.name) == c_ast.ID:
            if call.name.name in self.opts.funs_blacklist:
                return None
            func = self.builtin_call_map.get(call.name.name)
            if func is not None:
                return func(call, visitor)
        return None

    def get_constant(self, c):
        if c.type in ('int', 'unsigned int', 'long int',
                      'unsigned long int', 'long long int'):
            base = 10
            val = c.value.lower()
            is_signed = 'u' not in val and not c.type.startswith('unsigned ')
            bytes = 4
            if val.endswith('ll'):
                bytes = 8
            if val.startswith('0x'):
                base = 16
            elif val.startswith('0o'):
                base = 8
            elif val.startswith('0b'):
                base = 2
            m = re.match('^([0-9a-fox]+)[ul]*$', val)
            if not m:
                raise Exception("unsupported: %s" % _tostr(c))
            else:
                val = m.group(1)

            value = int(val, base)

            if self.opts.bitvectors:
                # find the smallest type in which the value fits
                # c.f. https://en.cppreference.com/w/c/language/integer_constant
                def min_val(bytes: int):
                    return -(2**(8*bytes - 1)) if is_signed else 0
                def max_val(bytes: int):
                    return 2**(8*bytes - 1) - 1 if is_signed \
                        else 2**(8*bytes) - 1
                while value > max_val(bytes) or value < min_val(bytes):
                    bytes *= 2

                tp = BVType(bytes * 8, is_signed)
            else:
                tp = IntType()
            return ConstExpr(tp, str(value))
        elif c.type in ('float', 'double', 'long double'):
            val = c.value.lower()
            if val.lower().endswith('f'):
                val = val[:-1]
                tp = FPType(8, 23)
            elif val.lower().endswith('l'):
                val = val[:-1]
                tp = FPType(15, 112)
            else:
                tp = FPType(11, 52)
            if not self.opts.bitvectors or self.opts.floats_as_reals:
                tp = RealType()
            return ConstExpr(tp, val)
        elif c.type == 'char':
            try:
                ov = c.value[1:-1] # strip off the '' quotes
                if ov.startswith('\\x'):
                    val = int(ov[2:], 16)
                elif ov.startswith('\\'):
                    val = ord(ov[1:])
                else:
                    val = ord(ov)
            except TypeError:
                self.unsupported(c)
            if self.opts.bitvectors:
                tp = BVType(8, 0)
            else:
                tp = IntType()
            return ConstExpr(tp, str(val))
        elif c.type in ('bool', '_Bool'):
            return ConstExpr(BoolType(), c.value.lower())
        elif c.type == 'string':
            return ConstExpr(SymbolType('string'), str(c.value))
        return None

    def get_type_promotion(self, a, b):
        if isinstance(a.type, RealType) \
               and isinstance(b.type, (IntType, BVType)):
            b = TypeCastExpr(a.type, b, 0)
        elif isinstance(b.type, RealType) \
                 and isinstance(a.type, (IntType, BVType)):
            a = TypeCastExpr(b.type, a, 0)
        elif isinstance(a.type, FPType) \
             and isinstance(b.type, (IntType, BVType)):
            b = TypeCastExpr(a.type, b, 0)
        elif isinstance(b.type, FPType) \
             and isinstance(a.type, (IntType, BVType)):
            a = TypeCastExpr(b.type, a, 0)
        elif isinstance(a.type, FPType) \
             and isinstance(b.type, FPType):
            if a.type.exponent >= b.type.exponent and \
               a.type.mantissa >= b.type.mantissa:
                b = TypeCastExpr(a.type, b, 0)
            else:
                assert a.type.exponent <= b.type.exponent
                assert a.type.mantissa <= b.type.mantissa
                a = TypeCastExpr(b.type, a, 0)
        elif type(a.type) != type(b.type):
            if isinstance(a.type, BoolType):
                a = TypeCastExpr(b.type, a, 0)
                return a, b
            elif isinstance(b.type, BoolType):
                b = TypeCastExpr(a.type, b, 0)
                return a, b
            elif isinstance(a.type, PointerType) and \
                 isinstance(b.type, (IntType, BVType)):
                b = TypeCastExpr(a.type, b, 1)
                return a, b
            elif isinstance(b.type, PointerType) and \
                 isinstance(a.type, (IntType, BVType)):
                a = TypeCastExpr(b.type, a, 1)
                return a, b
            elif isinstance(a.type, EnumType) and \
                 isinstance(b.type, (IntType, BVType)):
                a = TypeCastExpr(b.type, a, 0)
                return a, b
            elif isinstance(b.type, EnumType) and \
                 isinstance(a.type, (IntType, BVType)):
                b = TypeCastExpr(a.type, b, 0)
                return a, b
            elif isinstance(a.type, ArrayType) and \
                 isinstance(b.type, (IntType, BVType)):
                a = TypeCastExpr(PointerType(a.type.element), a, 1)
                b = TypeCastExpr(a.type, b, 1)
                return a, b
            elif isinstance(b.type, ArrayType) and \
                 isinstance(a.type, (IntType, BVType)):
                b = TypeCastExpr(PointerType(b.type.element), b, 1)
                a = TypeCastExpr(b.type, a, 1)
                return a, b
            # the following two cases are **temporary** work-around for SV-COMP
            # benchmarks that use conditional operator where one branch is void
            # and the other is non-void. This is not allowed in standard C.
            elif isinstance(a.type, VoidType) and \
                 not isinstance(b.type, VoidType):
                b = TypeCastExpr(VoidType(), b, 1)
                return a, b
            elif isinstance(b.type, VoidType) and \
                 not isinstance(a.type, VoidType):
                a = TypeCastExpr(VoidType(), a, 1)
                return a, b
            raise Exception("unsupported type promotion: %s, %s" % \
                            (a.type, b.type))
        elif isinstance(a.type, BVType):
            if a.type.bits < b.type.bits:
                a = TypeCastExpr(b.type, a, 0)
            elif a.type.bits > b.type.bits:
                b = TypeCastExpr(a.type, b, 0)
            elif a.type.is_signed != b.type.is_signed:
                if a.type.is_signed:
                    a = TypeCastExpr(BVType(a.type.bits, 0), a, 0)
                else:
                    b = TypeCastExpr(BVType(b.type.bits, 0), b, 0)
        elif isinstance(a.type, PointerType) and isinstance(b.type, PointerType):
            if isinstance(b.type.dereference, VoidType):
                b = TypeCastExpr(a.type, b, 1)
            elif isinstance(a.type.dereference, VoidType):
                a = TypeCastExpr(b.type, a, 1)
            elif isinstance(b.type.dereference, SymbolType):
                b = TypeCastExpr(a.type, b, 1)
            elif isinstance(a.type.dereference, SymbolType):
                a = TypeCastExpr(b.type, a, 1)
            else:
                a = TypeCastExpr(PointerType(VoidType()), a, 1)
                b = TypeCastExpr(PointerType(VoidType()), b, 1)
        elif isinstance(a.type, EnumType) and isinstance(b.type, EnumType):
            int_tp = BVType(self.opts.bitvectors, True) \
                if self.opts.bitvectors else IntType()
            a = TypeCastExpr(int_tp, a, 0)
            b = TypeCastExpr(int_tp, b, 0)
        elif not isinstance(a.type, (IntType, RealType)):
            raise NotImplementedError(str(a.type) + " -> " +  str(b.type))
        return a, b

    def setup_visitor(self, visitor):
        pass

    def _builtin_call_assert(self, call, visitor):
        def label_expr(name, purpose):
            res = LabelExpr(name)
            if self.opts.annotate_labels:
                res.annotations = [('label-type', purpose)]
            return res
        if not call.args or len(call.args.exprs) not in (1, 2):
            self.unsupported(call)
        cnt = self.assert_cnt
        self.assert_cnt += 1
        cond = visitor.visit(call.args.exprs[0])
        if not isinstance(cond.type, BoolType):
            cond = TypeCastExpr(BoolType(), cond, 0)
        if len(call.args.exprs) == 2:
            l = call.args.exprs[1]
            if type(l) != c_ast.Constant or l.type != 'string' \
                   or not l.value.startswith('"') \
                   or not l.value.endswith('"'):
                self.unsupported(call)
            errame = l.value[1:-1]
        else:
            errname = 'assert#%d' % cnt
        lbl = label_expr('ERROR{%s}' % errname, 'error')
        ann = [('error', errname if not self.opts.single_error else 'error')]
        lbl.annotations = ann if not lbl.annotations else lbl.annotations + ann

        if self.opts.builtin_assert_force_cond:
            othercond = AssumeExpr(cond)
        else:
            othercond = AssumeExpr(ConstExpr(True))
        c1 = label_expr('$assert.c1#%d' % cnt, 'cond')
        c2 = label_expr('$assert.c2#%d' % cnt, 'cond')
        if self.opts.use_condjump \
               and self.opts.builtin_assert_force_cond:
            return SeqExpr(CondJumpExpr(cond, c2),
                           c1, lbl, JumpExpr(lbl),
                           c2)
        else:
            return SeqExpr(JumpExpr(c1, c2),
                           SeqExpr(c1,
                                   AssumeExpr(OpExpr(OpExpr.NOT, cond)),
                                   lbl, JumpExpr(lbl)),
                           SeqExpr(c2, othercond))

    def _builtin_call_assume(self, call, visitor):
        if not call.args or len(call.args.exprs) != 1:
            self.unsupported(call)
        cond = visitor.visit(call.args.exprs[0])
        if not isinstance(cond.type, BoolType):
            cond = TypeCastExpr(BoolType(), cond, 0)
        return AssumeExpr(cond)

    def _builtin_call_havoc(self, call, visitor):
        if not call.args or len(call.args.exprs) != 1:
            self.unsupported(call)
        target = visitor.visit(call.args.exprs[0])
        return HavocExpr(target)

    def _builtin_call_exit(self, call, visitor):
        return AssumeExpr(ConstExpr(False))
 
    def _builtin_call_abort(self, call, visitor):
        return AssumeExpr(ConstExpr(False))
   
    def _builtin_call_memset(self, call, visitor):
        # we handle a specific type of memset, used for
        # zero-initializing structs
        if not call.args or len(call.args.exprs) != 3:
            self.unsupported(call)
        def memset_zero_assign(call, a, v, visitor):
            #a = visitor.visit(call.args.exprs[0])
            if isinstance(a, TypeCastExpr):
                a = a.expr
            if not isinstance(a, AddrOfExpr):
                #self.unsupported(call)
                return False, None
            a = a.lval
            #v = visitor.visit(call.args.exprs[1])
            if not isinstance(v, ConstExpr) or v.value != '0':
                #self.unsupported(call)
                return False, None
            s = call.args.exprs[2]
            if type(s) != c_ast.UnaryOp or s.op != 'sizeof':
                #self.unsupported(call)
                return False, None
            if type(s.expr) == c_ast.Typename:
                tp = visitor._get_type(s.expr.type)
            elif type(s.expr) == c_ast.ID and isinstance(a, VarExpr) \
                 and s.expr.name == a.var:
                tp = a.type
            else:
                #self.unsupported(call)
                return False, None
            if tp != a.type:
                #print('tp: %s, a.type: %s' % (tp, a.type))
                #self.unsupported(call)
                return False, None
            seq = visitor._zero_assign(a)
            return True, seq
            
        a = visitor.visit(call.args.exprs[0])
        v = visitor.visit(call.args.exprs[1])
        ok, res = memset_zero_assign(call, a, v, visitor)
        if ok:
            return res
        else:
            return HavocExpr(a)

    def _builtin_call_floor(self, call, visitor):
        cond = visitor.visit(call.args.exprs[0])
        return OpExpr(OpExpr.FLOOR, cond)

    def _builtin_call_ceil(self, call, visitor):
        cond = visitor.visit(call.args.exprs[0])
        return OpExpr(OpExpr.NEG,
                      OpExpr(OpExpr.FLOOR, OpExpr(OpExpr.NEG, cond)))

    def _builtin_call_isfinite(self, call, visitor):
        cond = visitor.visit(call.args.exprs[0])
        return OpExpr(OpExpr.ISFINITE, cond)

    def _builtin_call_isinf(self, call, visitor):
        cond = visitor.visit(call.args.exprs[0])
        return OpExpr(OpExpr.ISINF, cond)

    def _builtin_call_isnan(self, call, visitor):
        cond = visitor.visit(call.args.exprs[0])
        return OpExpr(OpExpr.ISNAN, cond)

    def _builtin_call_isnormal(self, call, visitor):
        cond = visitor.visit(call.args.exprs[0])
        return OpExpr(OpExpr.ISNORMAL, cond)

    def _builtin_call_issubnormal(self, call, visitor):
        cond = visitor.visit(call.args.exprs[0])
        return OpExpr(OpExpr.ISSUBNORMAL, cond)

    def _builtin_call_iszero(self, call, visitor):
        cond = visitor.visit(call.args.exprs[0])
        return OpExpr(OpExpr.ISZERO, cond)

    def _builtin_call_float_as_bits(self, call, visitor):
        val = visitor.visit(call.args.exprs[0])
        if isinstance(val.type, FPType):
            return TypeCastExpr(BVType(val.type.bits, 0), val, 1)
        return None

    def _builtin_call_bits_as_float(self, call, visitor):
        val = visitor.visit(call.args.exprs[0])
        if isinstance(val.type, BVType):
            bits2flt = {
                16 : FPType(5, 10),
                32 : FPType(8, 23),
                64 : FPType(11, 52)
                }
            ftp = bits2flt.get(val.type.bits)
            if ftp is not None:
                return TypeCastExpr(ftp, val, 1)
        return None

    def _builtin_call_error(self, call, visitor):
        if visitor.st.lookup(call.name.name):
            return None
        else:
            args = []
            if call.args:
                for a in call.args.exprs:
                    args.append(visitor.visit(a))
            f = ConstExpr(FunctionType([], []), call.name.name)
            call = CallExpr(f, [], [])
            if args:
                args.append(call)
                return SeqExpr(*args)
            else:
                return call

# end of class DefaultCRuntime


class Initializer(object):
    def __init__(self, opts, runtime, key, val):
        self.opts = opts
        self.runtime = runtime
        self.key = key
        self.val = val

    def get_assign(self, lval):
        orig = lval
        def _acast(i):
            tp = BVType(self.opts.bitvectors, True) \
                if self.opts.bitvectors else IntType()
            return TypeCastExpr(tp, i, False)
        first = True
        for f in self.key:
            if isinstance(lval.type, PointerType):
                if isinstance(f, c_ast.Constant):
                    c = self.runtime.get_constant(f)
                    lval = DerefExpr(OpExpr(OpExpr.ADD, lval,
                                            ConstExpr(lval.type, c.value)))
                else:
                    assert False, str(lval) + ' ' + str(lval.type) + ' ' + str(_tostr(f))
            elif isinstance(lval.type, RecordType):
                if isinstance(f, c_ast.ID):
                    lval = FieldExpr(lval, f.name)
                else:
                    assert isinstance(f, c_ast.Constant)
                    c = self.runtime.get_constant(f)
                    pos = int(c.value)
                    for i, f in enumerate(lval.type.fields):
                        if pos == i:
                            break
                    lval = FieldExpr(lval, f)
            elif isinstance(lval.type, ArrayType):
                c = self.runtime.get_constant(f)
                lval = ArrAtExpr(lval, _acast(c))
            else:
                assert first, str(lval)
                first = False
        return lval, self.val

    def __str__(self):
        return '%s := %s' % (list(map(str, self.key)), self.val)

# end of class Initializer
                    

class SymbolTable(object):
    def __init__(self):
        self.scopes = [{}]
        self.counters = [{}]
        # Associates to a scope a function name. For nested scopes,
        # the name of the outermost function is used.
        self.functions = [None]

    def push(self, name=None):
        self.scopes.append({})
        self.counters.append({})
        if name is None:
            name = self.functions[-1]
        self.functions.append(name)
            
    def pop(self):
        self.scopes.pop()
        self.counters.pop()
        self.functions.pop()

    def _lookup(self, name):
        for i in range(len(self.scopes), 0, -1):
            ret = self.scopes[i-1].get(name, None)
            if ret is not None:
                return ret, self.functions[i-1]
        return None

    def lookup(self, name):
        r = self._lookup(name)
        if r is not None:
            r = r[0]
        return r

    def _get_counter(self, name, dflt):
        for i in range(len(self.counters), 0, -1):
            ret = self.counters[i-1].get(name, None)
            if ret is not None:
                return ret
        return dflt

    def add(self, decl):
        prev = self.lookup(decl.name)
        if prev is not None:
            d_e = 'extern' in decl.storage
            p_e = 'extern' in prev.storage
            if d_e and not p_e:
                return
            elif not p_e and not d_e:
                self.counters[-1][decl.name] = \
                                             self._get_counter(decl.name, 0) + 1
        self.scopes[-1][decl.name] = decl

    def get_flat_name(self, name):
        #s = self.counters.get(name, None)
        s = self._get_counter(name, None)
        if s is not None:
            ret = name + '#%d' % s
        else:
            ret = name
        d = self._lookup(name)
        assert d is not None
        # if it is a static variable, then it prefix the name of the
        # function the variable belongs to.
        if 'static' in d[0].storage and d[1]:
            ret = d[1] + '::' + ret
        return ret

    def get_level(self, name):
        for i in range(len(self.scopes), 0, -1):
            ret = self.scopes[i-1].get(name, None)
            if ret is not None:
                return i - 1
        return -1

# end of class SymbolTable


class EnumInfo(object):
    def __init__(self, enums, const, val):
        self.enums = enums
        self.const = const
        self.intval = self._eval(val)

    def _eval(self, val):
        enums = self.enums
        opmap = {
            OpExpr.ADD : lambda args: args[0] + args[1],
            OpExpr.SUB : lambda args: args[0] - args[1],
            OpExpr.MUL : lambda args: args[0] * args[1],
            OpExpr.DIV : lambda args: args[0] / args[1],
            OpExpr.REM : lambda args: args[0] % args[1],
            OpExpr.NEG : lambda args: -args[0],
            }
        class EvalVisitor(IdentityVisitor):
            def visit_TypeCastExpr(self, e):
                v = self.visit(e.expr)
                assert isinstance(v, int), str(e.expr)
                return v
                
            def visit_ConstExpr(self, e):
                v = enums.get(e.value)
                if v is not None:
                    return int(v.intval)
                return int(e.value)
            
            def visit_OpExpr(self, e):
                args = []
                for a in e.args:
                    aa = self.visit(a)
                    assert isinstance(aa, int), str(a)
                    args.append(aa)
                return opmap[e.op](args)

        return EvalVisitor().visit(val)

# end of class EnumInfo

    
class KratosGenerator(c_ast.NodeVisitor):
    """Modified from c_generator.CGenerator"""
    def __init__(self, opts):
        self.opts = opts
        self.runtime = opts.runtime
        if self.runtime is None:
            self.runtime = DefaultCRuntime(self.opts)
        self.st = SymbolTable()
        self.typedefs = {}
        self.retvar = None
        self.endlabel = None
        self.looplabels = []
        self.loopcnt = 1
        self.locals = {}
        self.temp_cnt = 1
        self.tempmap = {}
        self.cond_var = None

        self.globalstmts = []
        self.globalset = {}
        self.functions = []
        self.struct_id = 0
        self.anon_structs = {}
        self.enums = {}
        self.calledfuncs = set()

        self.pragmas = []

        self.runtime.setup_visitor(self)

    def _label(self, name, purpose):
        res = LabelExpr(name)
        if self.opts.annotate_labels:
            res.annotations = [('label-type', purpose)]
        return res

    def _annotate(self, e, ann):
        if e.annotations:
            e.annotations += ann
        else:
            e.annotations = ann

    def _unsupported(self, node, msg=None):
        raise Unsupported(_tostr(node, msg))

    def _add_global(self, var, *instrs):
        if var is None or var.var not in self.globalset:
            if var is not None:
                self.globalset[var.var] = var
            for s in instrs:
                if s is not None:
                    self.globalstmts.append(s)

    def visit(self, node):
        method = 'visit_' + node.__class__.__name__
        return getattr(self, method, self.generic_visit)(node)

    def generic_visit(self, node):
        self._unsupported(node)

    def _tempvar(self, tp):
        v = self.tempmap.get(tp, None) if self.opts.reuse_temporaries else None
        if not v:
            v = VarExpr(tp, '$temp.%d' % self.temp_cnt)
            self.temp_cnt += 1
            self.tempmap[tp] = v
            self.locals[v.var] = v
        return v

    def _condvar(self):
        if not self.opts.reuse_temporaries:
            v = VarExpr(BoolType(), '$cond.%d' % self.temp_cnt)
            self.temp_cnt += 1
            self.locals[v.var] = v
            return v
            
        if self.cond_var is None:
            self.cond_var = VarExpr(BoolType(), '$cond')
            self.locals[self.cond_var.var] = self.cond_var
        return self.cond_var

    def visit_Constant(self, n):
        r = self.runtime.get_constant(n)
        if r is None:
            self._unsupported(n)
        return r

    def _getvar(self, name, check_nondet=True):
        d = self.st.lookup(name)
        if d is None:
            if name in self.enums:
                return self.enums[name].const
        assert d is not None, name
        n = self.st.get_flat_name(name)
        lvl = self.st.get_level(name)
        ret = VarExpr(self._get_type(d.type), n)
        static = 'static' in d.storage
        if lvl > 1:
            if not static and ret.var not in self.locals:
                self.locals[ret.var] = ret
        if check_nondet and self.opts.nondet_prefix \
               and n.startswith(self.opts.nondet_prefix):
            ret = CompExpr(HavocExpr(ret), ret)
        return ret

    def _get_anon_struct(self, fields, arrdims):
        key = (tuple(fields.items()), tuple(sorted(arrdims.items())))
        if key in self.anon_structs:
            return self.anon_structs[key]
        ret = 'anon.struct.%d' % self.struct_id
        self.struct_id += 1
        self.anon_structs[key] = ret
        return ret

    def _get_type(self, tp, structname=None):
        if type(tp) == c_ast.PtrDecl:
            ret = self.runtime.get_pointer_type(tp)
            if ret is None:
                base = self._get_type(tp.type)
                ret = PointerType(base)
            return ret
        elif type(tp) == c_ast.ArrayDecl:
            elem = self._get_type(tp.type)
            if tp.dim is None:
                return PointerType(elem)
            else:
                try:
                    sz = self._eval_to_int_const(tp.dim)
                except Unsupported:
                    sz = None #self.visit(tp.dim)
                return ArrayType(elem, sz)
        elif type(tp) == c_ast.FuncDecl:
            args = []
            if tp.args is not None:
                for a in tp.args.params:
                    t = self._get_type(a.type)
                    args.append(t)
            ret = [self._get_type(tp.type)]
            if len(args) == 1 and isinstance(args[0], VoidType):
                args = []
            if isinstance(ret[0], VoidType):
                ret = []
            return FunctionType(args, ret)
        else:
            if hasattr(tp, 'type'):
                tp = tp.type
            typ = type(tp)
            if typ == c_ast.IdentifierType:
                ret = self.runtime.get_basic_type(tp)
                if ret is not None:
                    return ret
                last = tp.names[-1]
                if last in self.typedefs:
                    return self.typedefs[last]
            elif typ == c_ast.Struct or typ == c_ast.Union:
                name = tp.name
                if name is None:
                    name = structname
                if tp.decls is not None:
                    fields = OrderedDict()
                    arrdims = {}
                    for d in tp.decls:
                        if d.bitsize is not None and self.opts.bitvectors:
                            is_signed = d.bitsize.type in ("int", "signed int")
                            bw = int(d.bitsize.value)
                            if bw == 0:
                                # allignment to the next allocation unit
                                self._unsupported(n)
                            tp = BVType(bw, is_signed)
                        else:
                            tp = self._get_type(d.type)

                        fields[d.name] = tp
                    if name is None:
                        name = self._get_anon_struct(fields, arrdims)

                    result = RecordType(name, fields, arrdims, typ==c_ast.Union)
                    if name is not None:
                        self.typedefs['struct.' + name] = result

                    return result
                elif name is None:
                    assert False
                elif 'struct.' + name in self.typedefs:
                    return self.typedefs['struct.' + name]
                else:
                    return SymbolType(name)
            elif typ == c_ast.Enum:
                name = tp.name
                if tp.values is not None:
                    enumtype = EnumType([e.name for e in tp.values.enumerators])
                    val = self.visit(c_ast.Constant('int', '0'))
                    minval, maxval = 0, 0
                    for e in tp.values.enumerators:
                        n = e.name
                        v = e.value
                        incr = self.visit(c_ast.Constant('int', '1'))
                        if v is not None:
                            val = self.visit(v)
                        const = ConstExpr(enumtype, n)
                        self.enums[n] = EnumInfo(self.enums, const, val)
                        if isinstance(val, ConstExpr):
                            try:
                                v = int(val.value)
                            except ValueError:
                                v = self.enums[n].intval
                            val = ConstExpr(val.type, str(int(v) + 1))
                            if minval is not None:
                                minval = min(minval, int(v))
                            if maxval is not None:
                                maxval = max(maxval, int(v))
                        else:
                            val = OpExpr(OpExpr.ADD, val, incr)
                            minval, maxval = None, None

                    enumvals = {}
                    for v in enumtype.values:
                        enumvals[v] = self.enums[v].intval
                    enumtype._enumvals = enumvals
                    return enumtype
                    
                elif name is not None and 'enum.' + name in self.typedefs:
                    return self.typedefs['enum.' + name]
            raise Exception("unsupported type: %s" % _tostr(tp))

    def _skip(self):
        return AssumeExpr(ConstExpr(True))

    def visit_ID(self, n):
        if n.name in ('false', 'true'):
            return ConstExpr(BoolType(), n.name)
        d = self.st.lookup(n.name)
        if d is not None and type(d.type) == c_ast.FuncDecl:
            return self._getfunc(n.name)
        return self._getvar(n.name)
    
    def visit_Pragma(self, n):
        val = n.string
        if val.startswith('kratos ') or val.startswith('KRATOS '):
            self.pragmas.append(val[7:].strip())
        return None

    def _get_array_type(self, d):
        while type(d.type) == c_ast.ArrayDecl:
            d = d.type
        return d.type

    def _get_array_dim(self, d):
        cur = 0
        dims = []
        while type(d.type) == c_ast.ArrayDecl:
            if d.type.dim is not None:
                dims.append(int(d.type.dim.value))
            else:
                dims.append(len(d.init.exprs))
            d = d.type
        dims.reverse()
        return dims

    def visit_ArrayRef(self, n):
        a = self.visit(n.name)
        i = self.visit(n.subscript)
        if isinstance(a.type, ArrayType):
            tp = BVType(self.opts.bitvectors, True) \
                if self.opts.bitvectors else IntType()
            return ArrAtExpr(a, TypeCastExpr(tp, i, False))
        else:
            if isinstance(i, ConstExpr):
                i = ConstExpr(a.type, i.value)
            else:
                i = TypeCastExpr(a.type, i, 1)
            if not isinstance(a.type, PointerType):
                a = TypeCastExpr(PointerType(a.type), a, True)
            try:
                return DerefExpr(OpExpr(OpExpr.ADD, a, i))
            except:
                assert False, str(a)

    def visit_StructRef(self, n):
        if type(n.field) != c_ast.ID:
            self._unsupported(n)
        s = self.visit(n.name)
        if n.type == '->':
            assert isinstance(s.type, PointerType)
            if isinstance(s.type.dereference, SymbolType):
                td = self.typedefs.get('struct.' + s.type.dereference.name)
                if td is not None:
                    s = TypeCastExpr(PointerType(td), s, True)
            s = DerefExpr(s)
        return FieldExpr(s, n.field.name)

    def _getfunc_type(self, funcdecl):
        tps = []
        if funcdecl.args:
            for da in funcdecl.args.params:
                if isinstance(da, c_ast.EllipsisParam):
                    t = EllipsisType()
                else:
                    t = self._get_type(da.type)
                tps.append(t)
        if len(tps) == 1 and isinstance(tps[0], VoidType):
            tps = []
        ret = [self._get_type(funcdecl.type)]
        if len(ret) == 1 and isinstance(ret[0], VoidType):
            ret = []
        return FunctionType(tps, ret)

    def _getfunc(self, name):
        fname = name
        if not isinstance(name, str):
            if type(name) != c_ast.ID:
                if type(name) == c_ast.UnaryOp and name.op == '*':
                    # call through a function pointer
                    return self.visit(name)
                elif type(name) == c_ast.ArrayRef:
                    res = self.visit(name)
                    return DerefExpr(res)
                self._unsupported(name)
            name = name.name
        if name in self.opts.funs_blacklist:
            self._unsupported(fname)
        d = self.st.lookup(name)
        if not d:
            raise Exception("function not found: %s" % name)
        if type(d.type) == c_ast.PtrDecl:
            return DerefExpr(
                ConstExpr(PointerType(self._getfunc_type(d.type.type)),
                          d.name))

        if type(d.type) != c_ast.FuncDecl:
            #self._unsupported(d)
            f = self.visit(fname)
            if isinstance(f.type, PointerType):
                return DerefExpr(f)
            else:
                self._unsupported(d)

        return ConstExpr(self._getfunc_type(d.type), d.name)

    def _coerce(self, a, tp):
        if a.type == tp:
            return a
        is_bitcast = 0
        if isinstance(tp, PointerType) \
               and isinstance(a, ConstExpr) \
               and isinstance(a.type, FunctionType):
            a = AddrOfExpr(a)
            if a.type == tp:
                return a
        if isinstance(tp, PointerType) and isinstance(a.type, ArrayType) \
           and tp.dereference == a.type.element:
            is_bitcast = 1
        return TypeCastExpr(tp, a, is_bitcast)

    def visit_FuncCall(self, n):
        ret = self.runtime.get_builtin_call(n, self)
        if ret is not None:
            return ret
                
        f = self._getfunc(n.name)
        args = []
        extra = []
        if n.args:
            assert len(n.args.exprs) == len(f.type.args) \
                or (f.type.args and isinstance(f.type.args[-1], EllipsisType)) \
                or not f.type.args, f
            nt = len(f.type.args)
            if nt and isinstance(f.type.args[-1], EllipsisType):
                nt -= 1
            for i, a in enumerate(n.args.exprs):
                aa = self.visit(a)
                # if i < nt and not isinstance(f.type.args[i], EllipsisType):
                #     aa = self._coerce(aa, f.type.args[i])
                # args.append(aa)
                if i < nt:
                    aa = self._coerce(aa, f.type.args[i])
                    args.append(aa)
                else:
                    extra.append(aa)
        if type(n.name) == c_ast.ID:
            self.calledfuncs.add(n.name.name)
        retvars = [self._tempvar(tp) for tp in f.type.ret]
        # if f.type.args and isinstance(f.type.args[-1], EllipsisType):
        #     f = ConstExpr(FunctionType([a.type for a in args], f.type.ret),
        #                   f.value + '#%d' % len(args))
        call = CallExpr(f, args, retvars)
        if not retvars and not extra:
            return call
        if not extra:
            return CompExpr(call, retvars[0])
        else:
            extra.append(call)
            if retvars:
                extra.append(retvars[0])
            return CompExpr(*extra)

    def visit_UnaryOp(self, n):
        if n.op == 'sizeof':
            op = self.visit(n.expr)
            if isinstance(op, Type):
                tp = op
            else:
                tp = op.type
            def getsz(tp):
                #sz = 1
                if isinstance(tp, ArrayType):
                    #sz = tp.size
                    #tp = tp.element
                    return tp.size * getsz(tp.element)
                bytes = 1
                if isinstance(tp, PointerType):
                    bytes = 4
                elif isinstance(tp, RecordType):
                    bytes = 0
                    for f in tp.fields:
                        bytes += getsz(tp.fields[f])
                elif isinstance(tp, EnumType):
                    bytes = 4
                elif isinstance(tp, BoolType):
                    bytes = 1
                elif not isinstance(tp, (BVType, FPType, IntType, RealType)):
                    self._unsupported(n, str(tp) + " " + str(op))
                elif isinstance(tp, (BVType, FPType)):
                    bytes = int(tp.bits / 8)
                else:
                    bytes = 4
                return bytes #sz * bytes
            ## sz = 1
            ## if isinstance(tp, ArrayType):
            ##     sz = tp.size
            ##     tp = tp.element
            ## bytes = 1
            ## if not isinstance(tp, (BVType, IntType)):
            ##     self._unsupported(n)
            ## if isinstance(tp, BVType):
            ##     bytes = tp.bits / 8
            ## else:
            ##     bytes = 4
            ## val = str(sz * bytes)
            val = str(getsz(tp))
            if self.opts.bitvectors:
                mtp = BVType(self.opts.bitvectors, False)
            else:
                mtp = IntType()
            return ConstExpr(mtp, val)
        
        operand = self.visit(n.expr)
        if n.op == 'p++':
            tmp = self._tempvar(operand.type)
            return CompExpr(AssignExpr(tmp, operand),
                            AssignExpr(operand,
                                       OpExpr(OpExpr.ADD, operand,
                                              ConstExpr(operand.type, '1'))),
                            tmp)
        elif n.op == 'p--':
            tmp = self._tempvar(operand.type)
            return CompExpr(AssignExpr(tmp, operand),
                            AssignExpr(operand,
                                       OpExpr(OpExpr.SUB, operand,
                                              ConstExpr(operand.type, '1'))),
                            tmp)
        elif n.op == '++':
            return CompExpr(AssignExpr(operand,
                                       OpExpr(OpExpr.ADD, operand,
                                              ConstExpr(operand.type, '1'))),
                            operand)
        elif n.op == '--':
            return CompExpr(AssignExpr(operand,
                                       OpExpr(OpExpr.SUB, operand,
                                              ConstExpr(operand.type, '1'))),
                            operand)
        elif n.op == '!':
            if not isinstance(operand.type, BoolType):
                operand = TypeCastExpr(BoolType(), operand, 0)
            return OpExpr(OpExpr.NOT, operand)
        elif n.op == '+':
            return operand
        elif n.op == '-':
            return OpExpr(OpExpr.NEG, operand)
        elif n.op == '~':
            # from C reference: the operator ~ performs integer promotions on
            # its only operand
            return OpExpr(OpExpr.BITNOT, self._integer_promotion(operand))
        elif n.op == '&':
            return AddrOfExpr(operand)
        elif n.op == '*':
            if isinstance(operand.type, ArrayType):
                tp = BVType(self.opts.bitvectors, True) \
                    if self.opts.bitvectors else IntType()
                return ArrAtExpr(operand, TypeCastExpr(tp, 0, False))
            else:
                return DerefExpr(operand)
        else:
            self._unsupported(n)

    def _integer_promotion(self, a):
        if not self.opts.bitvectors:
            return a

        tp = a.type
        bw = tp.bits

        # from C reference
        # (https://en.cppreference.com/w/c/language/conversion): Integer
        # promotion is the implicit conversion of a value of any integer type
        # with rank less or equal to rank of int or of a bit field of type
        # _Bool, int, signed int, unsigned int, to the value of type int or
        # unsigned int.
        #
        # If int can represent the entire range of values of
        # the original type (or the range of values of the original bit field),
        # the value is converted to type int. Otherwise the value is converted
        # to unsigned int.

        if bw >= 32:
            # unsigned/signed int or anything with greater rank
            return a

        # tp is either unsigned/signed char, unsigned/signed short; all can fit
        # into int

        return TypeCastExpr(BVType(32, True), a, 0)

    def _promote(self, a, b):
        if a.type == b.type:
            return a, b
        return self.runtime.get_type_promotion(a, b)

    def visit_BinaryOp(self, n):
        def mkf(op, boolcast=None):
            def f(a, b):
                a = self.visit(a)
                b = self.visit(b)
                if boolcast == 1:
                    if not isinstance(a.type, BoolType):
                        a = TypeCastExpr(BoolType(), a, 0)
                    if not isinstance(b.type, BoolType):
                        b = TypeCastExpr(BoolType(), b, 0)
                else:
                    # Both arguments to
                    # - binary arithmetic *, /, %, +, -
                    # - relational operators <, >, <=, >=, ==, !=
                    # - binary bitwise arithmetic &, ^, |,
                    # undergo integer promotion and are then converted to a
                    # common type
                    # (https://en.cppreference.com/w/c/language/conversion)

                    if isinstance(a.type, BVType) and a.type.bits < 32:
                        a = self._integer_promotion(a)
                    if isinstance(b.type, BVType) and b.type.bits < 32:
                        b = self._integer_promotion(b)

                    a, b = self._promote(a, b)
                    if boolcast == 2 and isinstance(a.type, BoolType):
                        tp = IntType() if not self.opts.bitvectors \
                             else BVType(32, True)
                        a = TypeCastExpr(tp, a, 0)
                        b = TypeCastExpr(tp, b, 0)
                return OpExpr(op, a, b)
            return f

        ops = {
            '+' : mkf(OpExpr.ADD, 2),
            '-' : mkf(OpExpr.SUB, 2),
            '*' : mkf(OpExpr.MUL, 2),
            '/' : mkf(OpExpr.DIV, 2),
            '%' : mkf(OpExpr.REM, 2),
            '&&' : mkf(OpExpr.AND, 1),
            '||' : mkf(OpExpr.OR, 1),
            '==' : mkf(OpExpr.EQUAL),
            '!=' : lambda a, b:
            OpExpr(OpExpr.NOT, OpExpr(OpExpr.EQUAL,
                                      *self._promote(self.visit(a),
                                                     self.visit(b)))),
            '<' : mkf(OpExpr.LT, 2),
            '<=' : mkf(OpExpr.LE, 2),
            '>': mkf(OpExpr.GT, 2),
            '>=': mkf(OpExpr.GE, 2),
            '<<': mkf(OpExpr.LSHIFT, 2),
            '>>': mkf(OpExpr.RSHIFT, 2),
            '&': mkf(OpExpr.BITAND, 2),
            '|': mkf(OpExpr.BITOR, 2),
            '^': mkf(OpExpr.BITXOR, 2),
            }
        if n.op not in ops:
            self._unsupported(n)
        return ops[n.op](n.left, n.right)

    def visit_Assignment(self, n):
        l = self.visit(n.lvalue)
        r = self.visit(n.rvalue)
        ops = {
            '+=': OpExpr.ADD,
            '-=': OpExpr.SUB,
            '*=': OpExpr.MUL,
            '/=': OpExpr.DIV,
            '%=': OpExpr.REM,
            '=': None,
            '&=': OpExpr.BITAND,
            '|=': OpExpr.BITOR,
            '^=': OpExpr.BITXOR,
            '<<=': OpExpr.LSHIFT,
            '>>=': OpExpr.RSHIFT,
            }
        if n.op not in ops:
            self._unsupported(n)
        op = ops[n.op]
        if op is None:
            val = r
        else:
            val = OpExpr(op, l, self._coerce(r, l.type))
        val = self._coerce(val, l.type)
        return CompExpr(AssignExpr(l, val), l)

    def _get_zero(self, tp):
        if isinstance(tp, BoolType):
            zero = 'false'
        elif isinstance(tp, EnumType):
            zero = tp.values[0]
        else:
            zero = '0'
        val = ConstExpr(tp, zero)
        return val

    def _zero_assign(self, expr):
        seq = []
        def zero_assign(e):
            if not isinstance(e.type, (RecordType, ArrayType)):
                seq.append(AssignExpr(e, self._get_zero(e.type)))
            elif isinstance(e.type, ArrayType):
                if self.opts.bitvectors:
                    tp = BVType(self.opts.bitvectors, True)
                else:
                    tp = IntType()
                for i in range(e.type.size):
                    idx = ConstExpr(tp, str(i))
                    zero_assign(ArrAtExpr(e, idx))
            else:
                for f in e.type.fields:
                    zero_assign(FieldExpr(e, f))
        zero_assign(expr)
        if not seq:
            return self._skip()
        if len(seq) == 1:
            return seq[0]
        else:
            return SeqExpr(*seq)

    def _eval_to_int_const(self, n):
        if type(n) == c_ast.Constant:
            c = self.runtime.get_constant(n)
            return int(c.value)
        elif type(n) == c_ast.ID \
                 and n.name in self.enums:
            return self.enums[n.name].intval
        else:
            e = self.visit(n)
            def convert(e):
                if isinstance(e, ConstExpr):
                    return int(e.value)
                elif isinstance(e, OpExpr):
                    args = [convert(a) for a in e.args]
                    opmap = {
                        OpExpr.ADD: lambda a: a[0] + a[1],
                        OpExpr.SUB: lambda a: a[0] - a[1],
                        OpExpr.NEG: lambda a: -a[0],
                        OpExpr.MUL: lambda a: a[0] * a[1],
                        OpExpr.DIV: lambda a: a[0] / a[1],
                        OpExpr.REM: lambda a: a[0] % a[1],
                        OpExpr.RSHIFT: lambda a: a[0] << a[1],
                        OpExpr.LSHIFT: lambda a: a[0] >> a[1],
                        }
                    try:
                        return opmap[e.op](args)
                    except (KeyError, IndexError):
                        self._unsupported(n)
                else:
                    self._unsupported(n)
            return convert(e)

    def visit_Decl(self, n):
        ## if 'extern' in n.storage:
        ##     if type(n.type) == c_ast.FuncDecl:
        ##         self.st.add(n)
        ##     return None

        if n.bitsize:
            self._unsupported(n)
        if type(n.type) == c_ast.Struct or type(n.type) == c_ast.Union:
            if n.type.name and n.type.decls is not None:
                tp = self._get_type(n)
                for key in self.typedefs:
                    td = self.typedefs[key]
                    if isinstance(td, SymbolType) and td.name == n.type.name:
                        self.typedefs[key] = tp
                        break
            return None

        if type(n.type) == c_ast.ArrayDecl and not n.type.dim and n.init:
            if type(n.init) == c_ast.InitList:
                n.type.dim = c_ast.Constant('int', str(len(n.init.exprs)))
            elif type(n.init) == c_ast.Constant and n.init.type == 'string':
                n.type.dim = c_ast.Constant('int', str(len(n.init.value)-1))

        self.st.add(n)

        if type(n.type) == c_ast.FuncDecl:
            return

        is_static = 'static' in n.storage
        is_extern = 'extern' in n.storage
        is_global = len(self.st.scopes) == 1 and type(n.type) != c_ast.FuncDecl

        if type(n.type) == c_ast.ArrayDecl:
            dim = None
            if n.type.dim:
                try:
                    dim = self._eval_to_int_const(n.type.dim)
                except Unsupported:
                    dim = None
            elif n.init and type(n.init) == c_ast.InitList:
                dim = len(n.init.exprs)
            elif n.init and type(n.init) == c_ast.Constant \
                 and n.init.type == 'string':
                dim = len(n.init.value)-1
            if dim is None:
                if n.init or is_static or is_global:
                    self._unsupported(n)
                else:
                    # we allow dynamic arrays on the stack
                    return None
            if n.init:
                if type(n.init) == c_ast.InitList:
                    init = n.init.exprs + [None] * (dim - len(n.init.exprs))
                elif type(n.init) == c_ast.Constant and n.init.type == 'string':
                    init = [c_ast.Constant('char', '"%s"' % c)
                            for c in n.init.value[1:-1]] + \
                           [None] * (dim - len(n.init.value))
                else:
                    self._unsupported(n)
            else:
                init = [None] * dim
            seq = []
            cells = []
            a = self._getvar(n.name)
            assert isinstance(a.type, ArrayType), str(a)
            if init[0] is not None:
                for i, e in enumerate(init):
                    if not self.opts.bitvectors:
                        tp = IntType()
                    else:
                        tp = BVType(self.opts.bitvectors, True)
                    v = ArrAtExpr(a, ConstExpr(tp, str(i)))
                    if e is None:
                        res = self._zero_assign(v)
                        seq.append(res)
                    elif type(e) == c_ast.InitList:
                        for i in self.visit_InitList(e):
                            lhs, rhs = i.get_assign(v)
                            seq.append(AssignExpr(lhs,
                                                  self._coerce(rhs, lhs.type)))
                    elif type(e) == c_ast.NamedInitializer:
                        v = a
                        for n in e.name:
                            j = self.visit(n)
                            v = ArrAtExpr(v, j)
                        # print(f'; HERE: name = {[str(_tostr(n)) for n in e.name]}')
                        # print(f';       expr = {_tostr(e.expr)}')
                        val = self.visit(e.expr)
                        if isinstance(val, Expr):
                            seq.append(AssignExpr(v, self._coerce(val, v.type)))
                        else:
                            # print(f'; HERE: {_tostr(e)} {v}, TP: {v.type}')
                            # for j in val:
                            #     print(f';  -> {j}')
                            for j in val:
                                lhs, rhs = j.get_assign(v)
                                seq.append(
                                    AssignExpr(lhs,
                                               self._coerce(rhs, lhs.type)))
                            #     print(f'; GOT: {seq[-1]}')
                            # print(f'; END')
                    elif type(e) == c_ast.Constant:
                        val = self.visit(e)
                        seq.append(AssignExpr(v, self._coerce(val, v.type)))
                    else:
                        self._unsupported(n)
            elif is_global and not is_extern and self.opts.initialize_globals:
                seq.append(self._zero_assign(a))
            if is_static or is_global:
                if a.var in self.globalset and seq:
                    # already declared, just initialize
                    self._add_global(None, *seq)
                else:
                    # define and possibly initialize
                    self._add_global(a, *seq)
                ret = None
            else:
                if len(seq) == 1:
                    ret = seq[0]
                elif seq:
                    ret = SeqExpr(*seq)
                else:
                    ret = None
        elif type(n.type) == c_ast.Enum:
            if n.type.name:
                self.typedefs['enum.' + n.type.name] = self._get_type(n)
            return None
        else:
            v = self._getvar(n.name)
            if n.init:
                if type(n.init) == c_ast.InitList:
                    ret = []
                    for i in self.visit_InitList(n.init):
                        lhs, rhs = i.get_assign(v)
                        ret.append(AssignExpr(lhs, self._coerce(rhs, lhs.type)))
                    if ret:
                        ret = SeqExpr(*ret)
                    else:
                        ret = None
                else:
                    val = self.visit(n.init)
                    ret = AssignExpr(v, self._coerce(val, v.type))
                if is_static or is_global:
                    self._add_global(v)
                    if ret is not None:
                        self._add_global(None, ret)
                    ret = None
            else:
                ret = None
                if is_global and not is_extern and self.opts.initialize_globals:
                    ret = self._zero_assign(v)
                if is_static or is_global:
                    if isinstance(v, CompExpr):
                        v = v.args[-1]
                    if v.var in self.globalset and ret is not None:
                        # already declared, just initialize
                        self._add_global(None, ret)
                    else:
                        # define and possibly initialize
                        self._add_global(v, ret)
                    ret = None
        return ret

    def visit_DeclList(self, n):
        s = []
        for d in n.decls:
            e = self.visit(d)
            if e:
                s.append(e)
        if s:
            return SeqExpr(*s)
        else:
            return None

    def visit_Typedef(self, n):
        tp = self._get_type(n.type, structname=n.name)
        if n.name in self.typedefs:
            prev = self.typedefs[n.name]
            if prev != tp and not \
               (isinstance(prev, SymbolType) and isinstance(tp, RecordType) \
                and prev.name == tp.name):
                raise Exception("typedef for %s already seen "
                                "(old: %s, new: %s)" %
                                (n.name, prev, tp))
        self.typedefs[n.name] = tp
        #print(f'; TYPE: {_tostr(n.type)}')
        if type(n.type) == c_ast.TypeDecl:
            if type(n.type.type) in (c_ast.Union, c_ast.Struct, c_ast.Enum) \
               and n.type.type.name:
                prefix = 'enum.' if type(n.type.type) == c_ast.Enum \
                    else 'struct.'
                self.typedefs[prefix + n.type.type.name] = tp
        return None

    def visit_Cast(self, n):
        a = self.visit(n.expr)
        t = self._get_type(n.to_type.type)
        if a.type == t:
            return a
        else:
            return TypeCastExpr(t, a, 0)

    def visit_ExprList(self, n):
        s = [self.visit(e) for e in n.exprs]
        if s:
            return SeqExpr(*s)
        else:
            return self._skip()

    def visit_InitList(self, n):
        ret = []
        for i, e in enumerate(n.exprs):
            ki = c_ast.Constant('int', str(i))
            r = self.visit(e)
            if type(e) == c_ast.NamedInitializer:
                #ret.append(r)
                ret += r
            elif type(e) == c_ast.InitList:
                for c in r:
                    ret.append(Initializer(self.opts,
                                           self.runtime, [ki] + c.key, c.val))
            else:
                ret.append(Initializer(self.opts, self.runtime, [ki], r))
        return ret
        #self._unsupported(n)

    def visit_Enum(self, n):
        self._unsupported(n)

    def visit_FuncDef(self, n):
        func = n
        funcname = func.decl.name
        if self.runtime.is_builtin(funcname) \
           or funcname in self.opts.funs_blacklist:
            return

        tp = self._get_type(func.decl.type.type)
        if not isinstance(tp, VoidType):
            self.retvar = VarExpr(tp, funcname + '.return')
        else:
            self.retvar = None
        self.looplabels = []
        self.loopcnt = 1
        self.locals = {}
        self.temp_cnt = 1
        self.tempmap = {}
        self.cond_var = None

        d = func.decl
        self.st.add(d)
        
        tps = []
        if d.type.args:
            for da in d.type.args.params:
                if isinstance(da, c_ast.EllipsisParam):
                    t = EllipsisType()
                else:
                    t = self._get_type(da.type)
                tps.append(t)
        self.st.push()
        if len(tps) == 1 and isinstance(tps[0], VoidType):
            params = []
        else:
            params = []
            if d.type.args:
                for i, p in enumerate(d.type.args.params):
                    if type(p) == c_ast.Decl:
                        self.st.add(p)
                        params.append(VarExpr(self._get_type(p.type),
                                              self.st.get_flat_name(p.name)))
                    elif type(p) == c_ast.Typename:
                        params.append(VarExpr(self._get_type(p.type),
                                              '%s.param.%d' % (funcname, i)))
                    elif isinstance(p, c_ast.EllipsisParam):
                        pass
                    else:
                        raise Exception("unsupported function parameter: %s" %
                                        p)
        self.st.push(funcname)

        returns = []
        if self.retvar:
            returns.append(self.retvar)
        if self.opts.nondet_prefix \
               and funcname.startswith(self.opts.nondet_prefix):
            if self.retvar:
                body = HavocExpr(self.retvar)
            else:
                body = self._skip()
            fdef = Function(funcname, params, returns, [], body)
        else:
            if funcname != self.opts.init_function:
                self.endlabel = self._label('%s.return.lbl' % funcname, 'return')
            body = self.visit(func.body)
            if funcname != self.opts.init_function:
                body = SeqExpr(body, self.endlabel)
            localvars = [self.locals[v] for v in sorted(self.locals.keys())]
            fdef = Function(funcname, params, returns, localvars, body)

        self.st.pop()
        self.st.pop()

        self.functions.append(fdef)
        

    def visit_FileAST(self, n):
        for e in n.ext:
            self.visit(e)

    def visit_Compound(self, n):
        self.st.push()
        sub = []
        if n.block_items:
            for st in n.block_items:
                s = self.visit(st)
                if s:
                    if type(st) == c_ast.Assignment \
                           and isinstance(s, CompExpr) and len(s.args) > 1:
                        s.args = s.args[:-1]
                        if len(s.args) == 1:
                            s = s.args[0]
                    sub.append(s)
        self.st.pop()
        if sub:
            return SeqExpr(*sub)
        else:
            return self._skip()

    def visit_EmptyStatement(self, n):
        return self._skip()

    def visit_ParamList(self, n):
        self._unsupported(n)

    def visit_Return(self, n):
        assert self.endlabel is not None
        if n.expr:
            if self.retvar:
                a = AssignExpr(self.retvar, self._coerce(self.visit(n.expr),
                                                         self.retvar.type))
            else:
                a = self.visit(n.expr)
            return SeqExpr(a, JumpExpr(self.endlabel))
        else:
            return JumpExpr(self.endlabel)

    def visit_Break(self, n):
        if not self.looplabels:
            self._unsupported(n)
        return JumpExpr(self._label('loop.end#%d' % self.looplabels[-1],
                                    'loop-break'))

    def visit_Continue(self, n):
        if not self.looplabels:
            self._unsupported(n)
        return JumpExpr(self._label('loop#%d' % self.looplabels[-1],
                                    'loop-continue'))

    def _getcond(self, cond):
        def has_sideffects(e):
            class SideEffectsVisitor(ExprVisitor):
                def __init__(self):
                    self.flag = False
                def visit_CompExpr(self, e):
                    self.flag = True
                    raise StopIteration
            v = SideEffectsVisitor()
            try:
                v.visit(e)
            except StopIteration:
                pass
            return v.flag
        if not isinstance(cond.type, BoolType):
            cond = TypeCastExpr(BoolType(), cond, 0)
        if has_sideffects(cond):
            v = self._condvar()
            return v, [AssignExpr(v, cond)]
        else:
            return cond, []

    def visit_TernaryOp(self, n):
        cond, asgn = self._getcond(self.visit(n.cond))
        t = self.visit(n.iftrue)
        e = self.visit(n.iffalse)
        t, e = self._promote(t, e)
        c = self._get_loop_label(False)
        l1 = self._label('$ite.then#%d' % c, 'cond')
        l2 = self._label('$ite.else#%d' % c, 'cond')
        l3 = self._label('$ite.end#%d' % c, 'cond')

        ret = self._tempvar(t.type)
        is_void = isinstance(t.type, VoidType)
        statement_t = t if is_void else AssignExpr(ret, t)
        statement_e = e if is_void else AssignExpr(ret, e)

        if self.opts.use_condjump:
            return CompExpr(*(asgn +
                              [CondJumpExpr(cond, l1),
                               SeqExpr(l2, statement_e, JumpExpr(l3)),
                               SeqExpr(l1, statement_t),
                               l3,
                               ret]))
        else:
            return CompExpr(*(asgn +
                              [JumpExpr(l1, l2),
                               SeqExpr(l1, AssumeExpr(cond), statement_t,
                                       JumpExpr(l3)),
                               SeqExpr(l2, AssumeExpr(OpExpr(OpExpr.NOT, cond)),
                                       statement_e),
                               l3,
                               ret]))

    def _get_loop_label(self, push=True):
        c = self.loopcnt
        self.loopcnt += 1
        if push:
            self.looplabels.append(c)
        return c

    def visit_If(self, n):
        c = self._get_loop_label(False)
        lthen = self._label('$ite.then#%d' % c, 'cond')
        lelse = self._label('$ite.else#%d' % c, 'cond')
        lend = self._label('$ite.end#%d' % c, 'cond')

        if self.opts.annotate_branches:
            lbl = c_generator.CGenerator().visit(n.cond)
            coord = str(n.cond.coord)
            self._annotate(lthen, [('branch', '[%s] %s' % (coord, lbl))])
            self._annotate(lelse, [('branch', '[%s] !(%s)' % (coord, lbl))])
        
        cond, asgn = self._getcond(self.visit(n.cond))
        ethen = SeqExpr(self.visit(n.iftrue), self._skip())
        if n.iffalse:
            eelse = SeqExpr(self.visit(n.iffalse), self._skip())
        else:
            eelse = self._skip()
        if self.opts.use_condjump:
            return SeqExpr(*(asgn + 
                             [CondJumpExpr(cond, lthen),
                              lelse,
                              eelse,
                              JumpExpr(lend),
                              lthen,
                              ethen,
                              lend]))
        else:
            return SeqExpr(*(asgn + 
                             [JumpExpr(lthen, lelse),
                              lthen,
                              AssumeExpr(cond),
                              ethen,
                              JumpExpr(lend),
                              lelse,
                              AssumeExpr(OpExpr(OpExpr.NOT, cond)),
                              eelse,
                              lend]))

    def visit_For(self, n):
        c = self._get_loop_label()
        init = self._skip()
        if n.init:
            init = self.visit(n.init)
        looplbl = self._label('loop#%d' % c, 'loop-continue')
        loopend = self._label('loop.end#%d' % c, 'loop-break')
        if n.cond:
            cond, asgn = self._getcond(self.visit(n.cond))
            if asgn:
                cond = CompExpr(*(asgn + [cond]))
        else:
            cond = ConstExpr(True)
        incr = self._skip()
        if n.next:
            # postfix increment/decrement have no side-effects here, we can
            # replace them with the prefix variant, which is translated better
            if isinstance(n.next, c_ast.UnaryOp):
                if n.next.op == 'p++':
                    n.next.op = '++'
                elif n.next.op == 'p--':
                    n.next.op = '--'
            #print('VISIT NEXT: %s, %s' % (n.next, type(n.next)))
            incr = self.visit(n.next)
        body = [
            self.visit(n.stmt),
            looplbl
            ]
        ret = SeqExpr(ForExpr(init, cond, incr, SeqExpr(*body)), loopend)
        self.looplabels.pop()
        return ret

    def visit_While(self, n):
        c = self._get_loop_label()
        looplbl = self._label('loop#%d' % c, 'loop-while')
        loopentry = self._label('loop.entry#%d' % c, 'loop-while')
        loopexit = self._label('loop.exit#%d' % c, 'loop-while')
        loopend = self._label('loop.end#%d' % c, 'loop-while')
        if n.cond:
            cond, asgn = self._getcond(self.visit(n.cond))
        else:
            cond, asgn = ConstExpr(True), []
        if self.opts.use_condjump:
            test = [looplbl] + asgn + [CondJumpExpr(cond, loopentry),
                                       JumpExpr(loopexit)]
            loop = [loopentry,
                    self.visit(n.stmt),
                    JumpExpr(looplbl)]
            post = [loopexit, loopend]
        else:
            test = [looplbl] + asgn + [JumpExpr(loopentry, loopexit)]
            loop = [loopentry,
                    AssumeExpr(cond),
                    self.visit(n.stmt),
                    JumpExpr(looplbl)]
            post = [loopexit, AssumeExpr(OpExpr(OpExpr.NOT, cond)), loopend]
        ret = SeqExpr(*(test + loop + post))
        self.looplabels.pop()
        return ret

    def visit_DoWhile(self, n):
        c = self._get_loop_label()
        looplbl = self._label('loop#%d' % c, 'loop-while')
        loopentry = self._label('loop.entry#%d' % c, 'loop-while')
        loopexit = self._label('loop.exit#%d' % c, 'loop-while')
        loopend = self._label('loop.end#%d' % c, 'loop-while')
        if n.cond:
            cond, asgn = self._getcond(self.visit(n.cond))
        else:
            cond, asgn = ConstExpr(True), []
        def simplify(c):
            if isinstance(c, TypeCastExpr):
                e = c.expr
                if isinstance(e, ConstExpr) \
                   and isinstance(e.type, (IntType, RealType, PointerType,
                                           BVType, FPType)):
                    try:
                        val = bool(float(e.value))
                        return ConstExpr(val)
                    except ValueError:
                        pass
            return c
        is_trivial = simplify(cond).equals(ConstExpr(False))
        if is_trivial:
            body_and_test = [self.visit(n.stmt)] + asgn
            loop = []
            post = [looplbl, loopend]
        elif self.opts.use_condjump:
            body_and_test = [looplbl, self.visit(n.stmt)] + \
                            asgn + \
                            [CondJumpExpr(cond, loopentry), JumpExpr(loopexit)]
            loop = [loopentry,
                    JumpExpr(looplbl)]
            post = [loopexit,
                    loopend]
        else:
            body_and_test = [looplbl, self.visit(n.stmt)] + \
                            asgn + \
                            [JumpExpr(loopentry, loopexit)]
            loop = [loopentry,
                    AssumeExpr(cond),
                    JumpExpr(looplbl)]
            post = [loopexit,
                    AssumeExpr(OpExpr(OpExpr.NOT, cond)),
                    loopend]
        ret = SeqExpr(*(body_and_test + loop + post))
        self.looplabels.pop()
        return ret

    def visit_Switch(self, n):
        loop_id = self._get_loop_label()
        cases = []
        #conds = []
        conds = {}
        body = []
        lhscond = self.visit(n.cond)
        lhs = self._tempvar(lhscond.type)
        
        if type(n.stmt) != c_ast.Compound:
            self._unsupported(n)
        self.st.push()
        ncases = len(n.stmt.block_items)
        switchbase = None
        cgen = None
        if self.opts.annotate_branches:
            cgen = c_generator.CGenerator()
            switchbase = 'switch (%s)' % cgen.visit(n.cond)
        has_default = False
        default_body = []
        for i, c in enumerate(n.stmt.block_items):
            if type(c) == c_ast.Case:
                cond = OpExpr(OpExpr.EQUAL,
                              *self._promote(lhs, self.visit(c.expr)))
                conds[i] = cond
                
        for i, c in enumerate(n.stmt.block_items):
            if type(c) == c_ast.Case:
                # cond = OpExpr(OpExpr.EQUAL,
                #               *self._promote(lhs, self.visit(c.expr)))
                cond = conds[i]
                le = self._label('case.cond#%d.%d' % (loop_id, i), 'switch-case')
                if self.opts.use_condjump:
                    l = [CondJumpExpr(OpExpr(OpExpr.NOT, cond), le)]
                else:
                    #conds.append(cond)
                    cases.append(le)
                    l = [le, AssumeExpr(cond)]
                l.append(self._label('case#%d.%d' % (loop_id, i), 'switch-case'))
                if self.opts.annotate_branches:
                    lbl = '[%s] %s, case %s' % (c.coord,
                                                switchbase, cgen.visit(c.expr))
                    self._annotate(l[-1], [('branch', lbl)])
                for stmt in c.stmts:
                    s = self.visit(stmt)
                    if s:
                        l.append(s)
                if i+1 < ncases:
                    l.append(JumpExpr(self._label('case#%d.%d' % (loop_id, i+1),
                                                  'switch-case')))
                else:
                    l.append(self._skip())
                if self.opts.use_condjump:
                    l.append(le)
                body += l
            elif type(c) == c_ast.Default:
                has_default = True
                if not self.opts.use_condjump:
                    if conds:
                        cond = OpExpr(OpExpr.NOT,
                                      reduce(lambda a, b:
                                             OpExpr(OpExpr.OR, a, b),
                                             [conds[j] for j in sorted(conds)]))
                    else:
                        cond = ConstExpr(True)
                le = self._label('case.default#%d.%d' % (loop_id, i),
                                 'switch-case')
                if self.opts.annotate_branches:
                    lbl = '[%s] %s, default' % (c.coord, switchbase)
                    self._annotate(le, [('branch', lbl)])
                if not self.opts.use_condjump:
                    cases.append(le)
                    l = [le, AssumeExpr(cond)]
                else:
                    l = []
                l.append(self._label('case#%d.%d' % (loop_id, i), 'switch-case'))
                for stmt in c.stmts:
                    s = self.visit(stmt)
                    if s:
                        l.append(s)
                l.append(self._skip())
                if i+1 == ncases:
                    body += l
                else:
                    rlbl = self._label('case.default-continue#%d.%d' %
                                       (loop_id, i), 'switch-case')
                    default_body += l
                    default_body.append(JumpExpr(rlbl))
                    body.append(rlbl)
            elif type(c) == c_ast.Decl:
                self.visit_Decl(c)
            else:
                self._unsupported(n)

        loop_end = self._label('loop.end#%d' % loop_id, 'switch-case')
        body += default_body

        if not self.opts.use_condjump and not has_default:
            # allow jumping over the switch statement if no case has matched

            # end the previous case in case it was not terminated by break
            body.append(JumpExpr(loop_end))

            # label for the no-match case
            le = self._label('case.no-match#%d' % loop_id, 'switch-case')
            if self.opts.annotate_branches:
                lbl = '[%s] %s, no-match' % (c.coord, switchbase)
                self._annotate(le, [('branch', lbl)])
            cases.append(le)
            body.append(le)

            # assume that no case has matched
            body.append(OpExpr(OpExpr.NOT,
                               reduce(lambda a, b: OpExpr(OpExpr.OR, a, b),
                                      #conds,
                                      [conds[j] for j in sorted(conds)],
                                      ConstExpr(False))))

            # do nothing and continue (next statement is loop-end; the end of
            # the switch statement)
            body.append(self._skip())

        self.looplabels.pop()

        if self.opts.use_condjump:
            ret = SeqExpr(*[AssignExpr(lhs, lhscond)] + body + [loop_end])
        else:
            ret = SeqExpr(*[AssignExpr(lhs, lhscond), JumpExpr(*cases)] +
                          body + [loop_end])
        self.st.pop()
        return ret

    def visit_Case(self, n):
        self._unsupported(n)

    def visit_Default(self, n):
        self._unsupported(n)

    def visit_Label(self, n):
        return SeqExpr(self._label(n.name, 'goto'), self.visit(n.stmt))

    def visit_Goto(self, n):
        return JumpExpr(self._label(n.name, 'goto'))

    def visit_EllipsisParam(self, n):
        self._unsupported(n)

    def visit_Struct(self, n):
        self._unsupported(n)

    def visit_Typename(self, n):
        #self._unsupported(n)
        return self._get_type(n.type)

    def visit_Union(self, n):
        self._unsupported(n)

    def visit_NamedInitializer(self, n):
        if type(n.expr) == c_ast.InitList:
            ret = []
            for i in self.visit_InitList(n.expr):
                ret.append(Initializer(self.opts, self.runtime,
                                       n.name + i.key, i.val))
            return ret
        else:
            return [Initializer(self.opts, self.runtime,
                                n.name, self.visit(n.expr))]

    def visit_CompoundLiteral(self, n):
        #print(f'; visiting: {_tostr(n)}')
        tp = self.visit(n.type)
        v = self._tempvar(tp)
        ret = []
        for i in self.visit_InitList(n.init):
            lhs, rhs = i.get_assign(v)
            ret.append(AssignExpr(lhs, self._coerce(rhs, lhs.type)))
        ret.append(v)
        return CompExpr(*ret)       

    def visit_FuncDecl(self, n):
        self._unsupported(n)

    def get(self):
        glbls = [self.globalset[name] for name in sorted(self.globalset)]
        init = self.globalstmts

        maincall = None
        initfunc = None
        errfunc = None
        defuns = set()
        functions = []
        
        for f in self.functions:
            defuns.add(f.name)
            functions.append(f)
            if f.name == self.opts.init_function:
                initfunc = f
                functions.pop()
            elif f.name == self.opts.entry_point:
                maincall = f
            elif f.name == self.opts.builtin_error:
                errfunc = f
                functions.pop()

        for f in self.calledfuncs - defuns:
            if self.opts.nondet_prefix \
               and f.startswith(self.opts.nondet_prefix):
                # generate the proper body for the function
                fc = self._getfunc(f)
                if fc.type.ret:
                    rv = VarExpr(fc.type.ret[0], f + '.return')
                    body = HavocExpr(rv)
                    fd = Function(f, [VarExpr(t, 'p%d' % i)
                                      for t in fc.type.args], [rv], [], body)
                    functions.append(fd)

        if errfunc is not None:
            errfunc.body = self._label('ERROR{%s}' % errfunc.name, 'error')
            functions.append(errfunc)
            self._annotate(errfunc.body,
                           [('error',
                             errfunc.name if not self.opts.single_error
                             else 'error')])
        elif self.opts.builtin_error:
            f = Function(self.opts.builtin_error, [], [], [],
                         self._label('ERROR{%s}' % self.opts.builtin_error,
                                     'error'))
            self._annotate(f.body, [('error',
                                     self.opts.builtin_error
                                     if not self.opts.single_error
                                     else 'error')])
            functions.append(f)

        if initfunc is not None:
            # extract the assume constraint from the init func
            if initfunc.params or initfunc.retvars or initfunc.locals:
                raise Exception('the init function '
                                'must not have params, locals or return vars: '
                                '%s' % initfunc.name)
            initconstr = initfunc.body
            if isinstance(initconstr, SeqExpr) and len(initconstr.args) == 1:
                initconstr = initconstr.args[0]
            if not isinstance(initconstr, AssumeExpr):
                raise Exception('the body of the init function must be an '
                                'assume expression: %s' % initfunc.name)
        else:
            initconstr = None

        prog = None
                    
        if self.opts.sldv_mode:
            inputset = None
            if self.opts.sldv_input_var:
                inputset = set(self.opts.sldv_input_var)
            def is_inputs(g):
                if inputset is not None:
                    if isinstance(g, VarExpr) and g.var in inputset:
                        return True
                else:
                    if isinstance(g.type, RecordType):
                        if self.opts.sldv_input_struct is not None:
                            return g.type.name == self.opts.sldv_input_struct
                        else:
                            return g.type.name.startswith('ExtU_')
                return False

            def is_initialize(f):
                if self.opts.sldv_initialize is not None:
                    return f.name == self.opts.sldv_initialize
                else:
                    return f.name.endswith('_initialize')

            def is_step(f):
                if self.opts.sldv_step is not None:
                    return f.name == self.opts.sldv_step
                else:
                    return f.name.endswith('_step')

            # search for inputs, init and step
            inputs = None
            initialize = None
            step = None

            inputs = [g for g in glbls if is_inputs(g)]
            initialize = [f for f in functions if is_initialize(f)]
            step = [f for f in functions if is_step(f)]

            if len(initialize) != 1:
                raise Exception("SLDV: %s initialize functions found:\n%s" %
                                ("multiple" if initialize else "no",
                                 "\n".join(f.name for f in functions)))
            if len(step) != 1:
                raise Exception("SLDV: %s step functions found" %
                                ("multiple" if step else "no"))

            body = SeqExpr(*([CallExpr(initialize[0].get_const(), [], []),
                              self._label('SLDV.loop', 'loop-while')] + 
                             [HavocExpr(i) for i in inputs] + 
                             [CallExpr(step[0].get_const(), [], []),
                              JumpExpr(self._label('SLDV.loop', 'loop-while'))]))
            sldvmain = Function('SLDV.main', [], [], [], body)
            functions.append(sldvmain)
            maincall = sldvmain

            prog = Program(glbls, initconstr, maincall.name, functions)
        else:
            if maincall:
                #if init:
                pvars = [VarExpr(v.type, 'param.%d' % i) for i, v in
                         enumerate(maincall.params)]
                rvars = [VarExpr(v.type, 'ret.%d' % i) for i, v in
                         enumerate(maincall.retvars)]
                init.append(CallExpr(maincall.get_const(), pvars, rvars))
                body = SeqExpr(*init)
                init_and_main = Function('`init.and.main', [], [],
                                         pvars + rvars, body)
                functions.append(init_and_main)
                maincall = init_and_main
                prog = Program(glbls, initconstr, maincall.name, functions)
            elif self.opts.entry_point:
                raise Exception('entry point not found: %s' %
                                self.opts.entry_point)
            else:
                prog = Program(glbls, initconstr, None, functions)

        if prog is not None and self.pragmas:
            prog.comment = '\n'.join(self.pragmas)
           
        return prog
        
# end of class KratosGenerator


_custom_header = '''\
#define __attribute__(X)
#define __restrict
#define __extension__
#define __inline
#define __inline__
#define __const
#define __asm__(a) ;
#define __PRETTY_FUNCTION__ ""
#define __signed__ signed
'''

def get_c_code(opts):
    includes = opts.cpp_includes[:]
    if opts.c_stubs_dir:
        includes.append(opts.c_stubs_dir)

    data = [_custom_header]

    if opts.svcomp_spec:
        data.append('#include "svcomp.h"')

    fdirs = set()
    if opts.filenames:
        for filename in opts.filenames:
            fdirs.add(os.path.dirname(filename))
            with open(filename) as f:
                data.append(f.read())
    else:
        data.append(sys.stdin.read())

    c_input = "\n".join(data)

    done = False
    if opts.cpp:
        def addflag(f, it, sep=True):
            if sep:
                return list(itertools.chain.from_iterable(
                    zip(itertools.repeat(f), it)))
            else:
                return list(f + e for e in it)

        fdir_argument = '-iquote' if opts.use_iquote_includes else '-I'
        cppcmd = opts.cpp.split() + \
                 addflag('-D', opts.cpp_defs, False) + \
                 addflag('-U', opts.cpp_undefs, False) + \
                 addflag('-I', includes) + \
                 addflag(fdir_argument, sorted(fdirs))

        # Note the use of universal_newlines to treat all newlines
        # as \n for Python's purpose
        #
        try:
            pipe = subprocess.Popen(cppcmd,
                                    stdout=subprocess.PIPE,
                                    stdin=subprocess.PIPE,
                                    universal_newlines=True)
            text = pipe.communicate(c_input)[0]
            done = True
        except:
            done = False

    if not done:
        import pcpp.preprocessor
        cpp = pcpp.preprocessor.Preprocessor()
        for d in quote_includes:
            cpp.add_path(os.path.abspath(d))
        for d in includes:
            cpp.add_path(os.path.abspath(d))
        for d in opts.cpp_defs:
            if '=' not in d:
                d += '=1'
            d = d.replace('=', ' ', 1)
            cpp.define(d)
        for d in opts.cpp_undefs:
            cpp.undef(d)
        cpp.parse(c_input)
        out = StringIO()
        cpp.write(out)
        text = out.getvalue()
        out.close()

    return text


def _c_parser(opts):
    class CLexer(c_lexer.CLexer):
        def t_ID(self, t):
            if t.value == '_Float16':
                t.type = 'FLOAT'
                return t
            elif opts.custom_int_types \
                 and DefaultCRuntime.kratos_inttype_re.match(t.value):
                t.type = 'INT'
                return t
            return super().t_ID(t)

    # end of class CLexer

    return c_parser.CParser(lexer=CLexer)


def parse_files(opts):
    text = get_c_code(opts)
    parser = _c_parser(opts)
    return parser.parse(text)


def parse_c_code(opts, text):
    parser = _c_parser(opts)
    return parser.parse(text)


def get_program(opts, ast):
    visitor = KratosGenerator(opts)
    visitor.visit(ast)
    return visitor.get()
