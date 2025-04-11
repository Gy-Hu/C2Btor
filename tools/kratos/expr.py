import copy
from io import StringIO

# currently, only expression manipulation is supported

Symbol = str

#############################################################################
# types
#############################################################################

class Type(object):
    "Base class for Kratos Type objects"
    __slots__ = []
    
    def write(self, out, bindings=None):
        "printer in K2 format"
        raise NotImplementedError

    def __eq__(self, other):
        return type(self) == type(other)

    def __ne__(self, other):
        return not (self == other)

    def __str__(self):
        "Get a string representation of this type in K2 format."
        buf = StringIO()
        self.write(buf)
        return buf.getvalue()

    def __hash__(self):
        return hash(type(self))

# end of class Type


class VoidType(Type):
    "void type"
    
    def write(self, out, bindings=None):
        out.write('void')

# end of class VoidType


class LabelType(Type):
    "label type"
    pass

class BoolType(Type):
    "boolean type"
    
    def write(self, out, bindings=None):
        out.write('bool')

# end of class BoolType


class IntType(Type):
    "integer type"
    
    def write(self, out, bindings=None):
        out.write('int')

# end of class IntType


class RealType(Type):
    "real type"
    
    def write(self, out, bindings=None):
        out.write('real')

# end of class RealType


class SymbolType(Type):
    "symbolic type"
    
    __slots__ = ['name']
    
    def __init__(self, name):
        self.name = name

    def __eq__(self, other):
        return isinstance(other, SymbolType) and self.name == other.name

    def write(self, out, bindings=None):
        out.write('(sym |%s|)' % self.name)

    def __hash__(self):
        return hash(self.name)

# end of class SymbolType


class BVType(Type):
    """fixed-size bit-vector type.
    Attributes:
    - bits: number of bits
    - is_signed: sign flag
    """
    
    __slots__ = ['bits', 'is_signed']
    
    def __init__(self, bits, is_signed):
        assert isinstance(bits, int)
        assert bits > 0
        self.bits = bits
        self.is_signed = bool(is_signed)

    def write(self, out, bindings=None):
        out.write('(%sbv %s)' % ('s' if self.is_signed else 'u', self.bits))

    def __eq__(self, other):
        return isinstance(other, BVType) and self.bits == other.bits \
               and self.is_signed == other.is_signed

    def __hash__(self):
        return super(BVType, self).__hash__() ^ self.bits ^ self.is_signed

# end of class BVType


class FPType(Type):
    """IEEE floating-point type.
    Attributes:
    - bits: total number of bits
    - exponent: number of exponent bits
    - mantissa: number of mantissa bits (without the implicit 1)
    """
    
    __slots__ = ['bits', 'exponent', 'mantissa']
    
    def __init__(self, e, m):
        self.bits = 1 + e + m
        self.exponent = e
        self.mantissa = m

    def __eq__(self, other):
        return isinstance(other, FPType) and self.exponent == other.exponent \
               and self.mantissa == other.mantissa

    def __hash__(self):
        return super(FPType, self).__hash__() ^ self.bits

    def write(self, out, bindings=None):
        out.write("(fp %s %s)" % (self.exponent, self.mantissa))

# end of class FPType


class MapType(Type):
    """map type.

    Attributes:
    - index: type of the index
    - element: type of the element"""
    
    __slots__ = ['index', 'element']
    
    def __init__(self, index, elem):
        self.index = index
        self.element = elem

    def __eq__(self, other):
        return isinstance(other, MapType) \
               and self.index == other.index \
               and self.element == other.element

    def __hash__(self):
        return super(MapType, self).__hash__() ^ hash(self.index) \
               ^ hash(self.element)

    def write(self, out, bindings=None):
        if bindings and self in bindings:
            out.write(bindings[self])
        else:
            out.write('(map ')
            self.index.write(out, bindings)
            out.write(' ')
            self.element.write(out, bindings)
            out.write(')')

# end of class MapType


class FunctionType(Type):
    """function type.
    Attributes:
    - args: list of argument types
    - ret: list of return types"""
    
    __slots__ = ['args', 'ret']
    
    def __init__(self, args, ret):
        self.args = args
        self.ret = ret

    def write(self, out, bindings=None):
        if bindings and self in bindings:
            out.write(bindings[self])
        else:
            out.write('(fun (')
            pad = ''
            for a in self.args:
                out.write(pad)
                pad = ' '
                a.write(out, bindings)
            out.write(') (')
            pad = ''
            for r in self.ret:
                out.write(pad)
                pad = ' '
                r.write(out, bindings)
            out.write('))')

    def __eq__(self, other):
        return isinstance(other, FunctionType) \
               and len(self.args) == len(other.args) \
               and len(self.ret) == len(other.ret) \
               and all(a == b for (a, b) in zip(self.args, other.args)) \
               and all(a == b for (a, b) in zip(self.ret, other.ret))

    def __hash__(self):
        h = 0
        for a in self.args:
            h ^= hash(a)
        for r in self.ret:
            h ^= hash(r)
        return h
    
# end of class FunctionType


class EnumType(Type):
    """symbolic enum type.
    Attributes:
    - values: list of enum values (as strings)"""
    
    __slots__ = ['values', '_enumvals']
    
    def __init__(self, values):
        self.values = sorted(values)

    def write(self, out, bindings=None):
        if bindings and self in bindings:
            out.write(bindings[self])
        else:
            out.write('(enum')
            for v in self.values:
                out.write(' |%s|' % v)
            out.write(')')

    def __eq__(self, other):
        return isinstance(other, EnumType) \
              and len(self.values) == len(other.values) \
              and all(a == b for (a, b) in zip(self.values, other.values))
        return ret

    def __hash__(self):
        h = hash(type(self))
        for v in self.values:
            h ^= hash(v)
        return h

# end of class EnumType
            

#############################################################################
# expressions
#############################################################################

class Expr(object):
    """
    Base class for Kratos expressions.
    Attributes:
    - type: the expression type
    - annotations: if not None, a list of (key, value) pairs
    """
    __slots__ = ['type', 'annotations']

    @staticmethod
    def _quote(s):
        if s is None:
            return "||"
        if s.isalnum():
            return s
        elif s and s[0] == '"' and s[-1] == '"':
            bad = False
            for i in range(1, len(s)-1):
                if s[i] in ('|', '"'):
                    bad = True
                    break
            if not bad:
                return s
        buf = ['|']
        for c in s:
            if c == '|':
                buf.append('\\')
            buf.append(c)
        buf.append('|')
        return "".join(buf)

    @staticmethod
    def _tosexp(s):
        if isinstance(s, tuple) and len(s) == 2:
            return '(%s . %s)' % (Expr._tosexp(s[0]), Expr._tosexp(s[1]))
        elif isinstance(s, (list, tuple)):
            return '(%s)' % ' '.join(map(Expr._tosexp, s))
        else:
            return Expr._quote(s)

    @staticmethod
    def _annotate(func):
        def f(self, out, indent=None, bindings=None):
            ann = None if not self.annotations \
                else [t for t in self.annotations if not t[0].startswith('.')]
            if ann:
                if indent is not None:
                    out.write('  ' * indent)
                indent = 0
                out.write('(! ')
            func(self, out, indent, bindings)
            if ann:
                for t in ann:
                    out.write(' :%s %s' % (t[0], Expr._tosexp(t[1])))
                out.write(')')
        return f
    
    def __init__(self, tp):
        self.type = tp
        self.annotations = None

    def write(self, out, indent=None, bindings=None):
        """
        Generate a K2 representation of this expression.
        Parameters:
        - out: file-like object
        - indent: (optional) indentation level
        - bindings: (optional) mapping from names to expressions
        """
        raise NotImplementedError

    def __str__(self):
        """
        Get a string representation of this type in K2 format.
        """
        buf = StringIO()
        self.write(buf)
        return buf.getvalue()

    def children(self):
        """
        Return the list of children of this expression.
        """
        raise NotImplementedError

    def equals(self, other):
        return self == other

# end of class Expr


class VarExpr(Expr):
    """A var expression.
    Attributes:
    - var: the variable name
    """
    __slots__ = ['var']
    
    def __init__(self, tp, var):
        super(VarExpr, self).__init__(tp)
        self.var = var

    @Expr._annotate
    def write(self, out, indent=None, bindings=None):
        if indent is not None:
            out.write('  ' * indent)
        if bindings is not None and self.var in bindings:
            out.write(Expr._quote(self.var))
        else:
            out.write('(var %s ' % Expr._quote(self.var))
            self.type.write(out, bindings)
            out.write(')')

    def children(self):
        return []

    def equals(self, other):
        return isinstance(other, VarExpr) \
               and self.type == other.type \
               and self.var == other.var

# end of class VarExpr


class ConstExpr(Expr):
    """
    A const expression.
    Attributes:
    - is_symbol: true if the value is a symbol
    - value: the expression value
    """
    __slots__ = ['value', 'is_symbol']
    
    def __init__(self, tp, value=None):
        if value is None:
            if isinstance(tp, bool):
                value = 'true' if tp else 'false'
                tp = BoolType()
            else:
                raise TypeError("ConstExpr: invalid value for tp argument: %s"
                                % tp)
        super(ConstExpr, self).__init__(tp)
        self.value = value
        self.is_symbol = not hasattr(value, 'items')

    @Expr._annotate
    def write(self, out, indent=None, bindings=None):
        if indent is not None:
            out.write('  ' * indent)
        if self.is_symbol and bindings is not None and self.value in bindings:
            out.write(Expr._quote(str(self.value)))
        elif self.is_symbol and isinstance(self.type, BoolType):
            out.write(self.value)
        else:
            out.write('(const ')
            if self.is_symbol:
                out.write(Expr._quote(str(self.value)))
            else:
                def prassoc(val):
                    if hasattr(val, 'items'):
                        out.write('( ')
                        sep = ''
                        for k, v in val.items():
                            out.write(sep)
                            out.write('(')
                            if k is not None:
                                out.write(Expr._quote(k))
                            else:
                                out.write('()')
                            out.write(' . ')
                            prassoc(v)
                            out.write(') ')
                        out.write(')')
                    elif isinstance(val, Expr):
                        val.write(out)
                    else:
                        out.write(Expr._quote(val))
                prassoc(self.value)
            out.write(' ')
            self.type.write(out, bindings)
            out.write(')')

    def children(self):
        return []

    def equals(self, other):
        return isinstance(other, ConstExpr) \
               and self.type == other.type \
               and self.value == other.value

# end of class ConstExpr
        

class OpExpr(Expr):
    """An op expression.
    Attributes:
    - op: the operation tag
    - args: the operation arguments
    """
    __slots__ = ['op', 'args']
    
    (ADD,
     SUB,
     MUL,
     NEG,
     DIV,
     REM,
     LSHIFT,
     RSHIFT,
     BITAND,
     BITOR,
     BITXOR,
     BITNOT,
     EQUAL,
     AND,
     OR,
     NOT,
     LE,
     LT,
     GE,
     GT,
     FLOOR,
     ISFINITE,
     ISINF,
     ISNAN,
     ISNORMAL,
     ISSUBNORMAL,
     ISZERO,
     MAPGET,
     MAPSET,
     ITE) = range(30)

    opstr = {
        ADD : 'add',
        SUB : 'sub',
        MUL : 'mul',
        NEG : 'neg',
        DIV : 'div',
        REM : 'rem',
        LSHIFT : 'lshift',
        RSHIFT : 'rshift',
        BITAND : 'bitand',
        BITOR : 'bitor',
        BITXOR : 'bitxor',
        BITNOT : 'bitnot',
        EQUAL : 'eq',
        AND : 'and',
        OR : 'or',
        NOT : 'not',
        LE : 'le',
        LT : 'lt',
        GE : 'ge',
        GT : 'gt',
        FLOOR : 'floor',
        ISFINITE : 'isfinite',
        ISINF : 'isinf',
        ISNAN : 'isnan',
        ISNORMAL : 'isnormal',
        ISSUBNORMAL : 'issubnormal',
        ISZERO : 'iszero',
        MAPGET : 'mapget',
        MAPSET : 'mapset',
        ITE : 'ite',
        }

    def __init__(self, op, *args):
        if len(args) == 1 and isinstance(args[0], (list, tuple)):
            args = args[0]
        assert op in self.opstr
        if op in (self.EQUAL, self.AND, self.OR, self.NOT,
                  self.LE, self.LT, self.GE, self.GT,
                  self.ISFINITE, self.ISINF, self.ISNAN,
                  self.ISNORMAL, self.ISSUBNORMAL, self.ISZERO):
            tp = BoolType()
        elif op == self.FLOOR:
            tp = IntType()
        elif op == self.ITE:
            tp = args[1].type
        else:
            tp = args[0].type
        super(OpExpr, self).__init__(tp)
        self.op = op
        self.args = list(args)

    @Expr._annotate
    def write(self, out, indent=None, bindings=None):
        if indent is not None:
            out.write('  ' * indent)
        out.write('(%s' % self.opstr[self.op])
        for a in self.args:
            out.write(' ')
            a.write(out, bindings=bindings)
        out.write(')')

    def children(self):
        return self.args

    def equals(self, other):
        return isinstance(other, OpExpr) \
               and self.op == other.op \
               and len(self.args) == len(other.args) \
               and all(a.equals(b) for (a, b) in zip(self.args, other.args))

# end of class OpExpr


class CallExpr(Expr):
    """
    A call expression.
    Attributes:
    - func: the function to call
    - args: the list of call arguments
    - ret: the list of return vars
    """
    __slots__ = ['func', 'args', 'ret']
    
    def __init__(self, func, args, ret):
        super(CallExpr, self).__init__(VoidType())
        self.func = func
        self.args = args
        self.ret = ret
        if not isinstance(self.args, list):
            self.args = list(self.args)
        if not isinstance(self.ret, list):
            self.ret = list(self.ret)

    @Expr._annotate
    def write(self, out, indent=None, bindings=None):
        if indent is not None:
            out.write('  ' * indent)
        out.write('(call ')
        self.func.write(out, bindings=bindings)
        for a in self.args:
            out.write(' ')
            a.write(out, bindings=bindings)
        for r in self.ret:
            out.write(' ')
            r.write(out, bindings=bindings)
        out.write(')')

    def children(self):
        return [self.func] + list(self.args) + list(self.ret)

    def equals(self, other):
        return isinstance(other, CallExpr) \
               and self.func.equals(other.func) \
               and len(self.args) == len(other.args) \
               and len(self.ret) == len(other.ret) \
               and all(a.equals(b) for (a, b) in
                       zip(self.args, other.args)) \
               and all(a.equals(b) for (a, b) in zip(self.ret, other.ret))

# end of class CallExpr


class AssignExpr(Expr):
    """
    An assign expression.
    Attributes:
    - lhs: the assignment target var
    - rhs: the assigned value
    """
    __slots__ = ['lhs', 'rhs']
    
    def __init__(self, lhs, rhs):
        super(AssignExpr, self).__init__(VoidType())
        self.lhs = lhs
        self.rhs = rhs

    @Expr._annotate
    def write(self, out, indent=None, bindings=None):
        if indent is not None:
            out.write('  ' * indent)
        out.write('(assign ')
        self.lhs.write(out, bindings=bindings)
        out.write(' ')
        self.rhs.write(out, bindings=bindings)
        out.write(')')

    def children(self):
        return [self.lhs, self.rhs]

    def equals(self, other):
        return isinstance(other, AssignExpr) \
               and self.lhs.equals(other.lhs) \
               and self.rhs.equals(other.rhs)

# end of class AssignExpr


class AssumeExpr(Expr):
    """
    An assume expression.
    Attributes:
    - cond: the assumption constraint
    """
    __slots__ = ['cond']
    
    def __init__(self, cond):
        super(AssumeExpr, self).__init__(VoidType())
        self.cond = cond

    @Expr._annotate
    def write(self, out, indent=None, bindings=None):
        if indent is not None:
            out.write('  ' * indent)
        out.write('(assume ')
        self.cond.write(out, bindings=bindings)
        out.write(')')

    def children(self):
        return [self.cond]

    def equals(self, other):
        return isinstance(other, AssumeExpr) \
               and self.cond.equals(other.cond)

# end of class AssumeExpr
        

class TypeCastExpr(Expr):
    """
    A typecast expression.
    Attributes:
    - expr: the child expression
    - is_bitcast: true if this is a bitcast
    """
    __slots__ = ['expr', 'is_bitcast']
    
    def __init__(self, tp, expr, is_bitcast):
        super(TypeCastExpr, self).__init__(tp)
        self.expr = expr
        self.is_bitcast = is_bitcast

    @Expr._annotate
    def write(self, out, indent=None, bindings=None):
        if indent is not None:
            out.write('  ' * indent)
        out.write('(%scast ' % ('bit' if self.is_bitcast else ''))
        self.type.write(out, bindings)
        out.write(' ')
        self.expr.write(out, bindings=bindings)
        out.write(')')

    def children(self):
        return [self.expr]

    def equals(self, other):
        return isinstance(other, TypeCastExpr) \
               and self.type == other.type \
               and self.expr.equals(other.expr) \
               and self.is_bitcast == other.is_bitcast

# end of class TypeCastExpr


class SeqExpr(Expr):
    """
    A seq expression.
    Attributes:
    - args: the list of children
    """
    __slots__ = ['args']
    
    def __init__(self, *args):
        if len(args) == 1 and isinstance(args[0], (list, tuple)):
            args = args[0]
        super(SeqExpr, self).__init__(args[-1].type)
        self.args = list(args)

    @Expr._annotate
    def write(self, out, indent=None, bindings=None):
        if indent is not None:
            out.write('  ' * indent)
            indent += 1
        out.write('(seq')
        for a in self.args:
            out.write('\n')
            a.write(out, indent, bindings)
        out.write(')')

    def children(self):
        return self.args

    def equals(self, other):
        return isinstance(other, SeqExpr) \
               and len(self.args) == len(other.args) \
               and all(a.equals(b) for (a, b) in zip(self.args, other.args))

# end of class SeqExpr


class JumpExpr(Expr):
    """
    A jump expression.
    Attributes:
    - targets: the list of targets
    """
    __slots__ = ['targets']
    
    def __init__(self, *targets):
        if len(targets) == 1 and isinstance(targets[0], (list, tuple)):
            targets = targets[0]
        super(JumpExpr, self).__init__(VoidType())
        self.targets = list(targets)

    @Expr._annotate
    def write(self, out, indent=None, bindings=None):
        if indent is not None:
            out.write('  ' * indent)
        out.write('(jump')
        for t in self.targets:
            out.write(' ')
            t.write(out, bindings=bindings)
        out.write(')')

    def children(self):
        return list(self.targets)

    def equals(self, other):
        return isinstance(other, JumpExpr) \
               and len(self.targets) == len(other.targets) \
               and all(a.equals(b) for (a, b) in
                       zip(self.targets, other.targets))

# end of class JumpExpr


class LabelExpr(Expr):
    """
    A label expression.
    Attributes:
    - name: the name of the label
    """
    __slots__ = ['name']
    
    def __init__(self, name):
        super(LabelExpr, self).__init__(LabelType())
        self.name = name

    @Expr._annotate
    def write(self, out, indent=None, bindings=None):
        if indent is not None:
            out.write('  ' * indent)
        out.write('(label %s)' % Expr._quote(self.name))

    def children(self):
        return []

    def equals(self, other):
        return isinstance(other, LabelExpr) \
               and self.name == other.name

# end of class LabelExpr


class HavocExpr(Expr):
    """
    A havoc expression.
    Attributes:
    - lval: the target variable
    """
    __slots__ = ['lval']
    
    def __init__(self, lval):
        super(HavocExpr, self).__init__(VoidType())
        self.lval = lval

    @Expr._annotate
    def write(self, out, indent=None, bindings=None):
        if indent is not None:
            out.write('  ' * indent)
        out.write('(havoc ')
        self.lval.write(out, bindings=bindings)
        out.write(')')

    def children(self):
        return [self.lval]

    def equals(self, other):
        return isinstance(other, HavocExpr) \
               and self.lval.equals(other.lval)

# end of class HavocExpr


class CondJumpExpr(Expr):
    """
    A condjump expression.
    Attributes:
    - cond: the jump condition
    - target: the jump target
    """
    __slots__ = ['cond', 'target']
    
    def __init__(self, cond, target):
        super(CondJumpExpr, self).__init__(VoidType())
        self.cond = cond
        self.target = target

    @Expr._annotate
    def write(self, out, indent=None, bindings=None):
        if indent is not None:
            out.write('  ' * indent)
        out.write('(condjump ')
        self.cond.write(out, bindings=bindings)
        out.write(' ')
        self.target.write(out, bindings=bindings)
        out.write(')')

    def children(self):
        return [self.cond, self.target]

    def equals(self, other):
        return isinstance(other, CondJumpExpr) \
               and self.cond.equals(other.cond) \
               and self.target.equals(other.target)

# end of class CondJumpExpr


class ExprVisitor(object):
    """
    A generic visitor for expressions.
    The visitor works by calling a method visit_ClassName for visiting expression
    of type `ClassName`. If no such method exists, `generic_visit` is called.
    The methods must accept a single argument, the expression to visit.
    Each method should call `self.visit(c)` recursively on each child `c` that
    needs to be visited.
    """
    def visit(self, expr):
        """visitor entry point.
        Parameters:
        - expr: the expression to visit
        """
        f = getattr(self, 'visit_' + expr.__class__.__name__,
                    self.generic_visit)
        return f(expr)

    def generic_visit(self, expr):
        """generic visitor function, which simply visits all the children
        of the input expression.
        Parameters:
        - expr: the expression to visit
        """
        for c in expr.children():
            self.visit(c)

# end of class ExprVisitor


class IdentityVisitor(ExprVisitor):
    """
    A subclass of `ExprVisitor` that builds a copy of the input expression
    """
    def visit(self, e):
        res = super().visit(e)
        if res is not e and isinstance(res, type(e)):
            res.annotations = copy.copy(e.annotations)
        return res
            
    def visit_VarExpr(self, e):
        return e

    def visit_ConstExpr(self, e):
        return e

    def visit_OpExpr(self, e):
        return OpExpr(e.op, *[self.visit(a) for a in e.args])

    def visit_CallExpr(self, e):
        return CallExpr(self.visit(e.func),
                        [self.visit(a) for a in e.args],
                        [self.visit(r) for r in e.ret])

    def visit_AssignExpr(self, e):
        return AssignExpr(self.visit(e.lhs), self.visit(e.rhs))

    def visit_AssumeExpr(self, e):
        return AssumeExpr(self.visit(e.cond))

    def visit_TypeCastExpr(self, e):
        return TypeCastExpr(e.type, self.visit(e.expr), e.is_bitcast)

    def visit_SeqExpr(self, e):
        return SeqExpr(*[self.visit(c) for c in e.args])

    def visit_JumpExpr(self, e):
        return JumpExpr(*[self.visit(t) for t in e.targets])

    def visit_LabelExpr(self, e):
        return e

    def visit_HavocExpr(self, e):
        return HavocExpr(self.visit(e.lval))

    def visit_CondJumpExpr(self, e):
        return CondJumpExpr(self.visit(e.cond), self.visit(e.target))

# end of class IdentityVisitor


class InplaceIdentityVisitor(IdentityVisitor):
    """
    A subclass of IdentityVisitor that modifies the input expression in-place.
    """
    def visit_VarExpr(self, e):
        return e

    def visit_ConstExpr(self, e):
        return e

    def visit_OpExpr(self, e):
        for i, a in enumerate(e.args):
            e.args[i] = self.visit(a)
        return e

    def visit_CallExpr(self, e):
        e.func = self.visit(e.func)
        for i, a in enumerate(e.args):
            e.args[i] = self.visit(a)
        for i, a in enumerate(e.ret):
            e.ret[i] = self.visit(a)
        return e

    def visit_AssignExpr(self, e):
        e.lhs = self.visit(e.lhs)
        e.rhs = self.visit(e.rhs)
        return e

    def visit_AssumeExpr(self, e):
        e.cond = self.visit(e.cond)
        return e

    def visit_TypeCastExpr(self, e):
        e.expr = self.visit(e.expr)
        return e

    def visit_SeqExpr(self, e):
        for i, c in enumerate(e.args):
            e.args[i] = self.visit(c)
        return e

    def visit_JumpExpr(self, e):
        for i, t in enumerate(e.targets):
            e.targets[i] = self.visit(t)
        return e

    def visit_HavocExpr(self, e):
        e.lval = self.visit(e.lval)
        return e

    def visit_CondJumpExpr(self, e):
        e.cond = self.visit(e.cond)
        e.target = self.visit(e.target)
        return e

# end of class InplaceIdentityVisitor


#############################################################################


class Function(object):
    """
    A Kratos function.
    Attributes:
    - name: the name of the function
    - params: the function formal parameters (list of var expressions)
    - retvars: the function return vars (list of var expressions)
    - locals: the funciton local vars (list of var expressions)
    - body: the function body
    """
    def __init__(self, name, params, retvars, locals, body):
        self.name = name
        self.params = params
        self.retvars = retvars
        self.locals = locals
        self.body = body
        self.comment = None
        self.annotations = None

    def __str__(self):
        """
        Get a string representation of this function in K2 format.
        """
        buf = StringIO()
        self.write(buf)
        return buf.getvalue()

    def write(self, out, bindings=None):
        """
        Write a K2 representation of the function to the file-like `out`.
        Parameters:
        - out: a file-like object
        - bindings: (optional) a mapping from names to expressions
        """
        if bindings is not None:
            bindings = copy.copy(bindings)
        else:
            bindings = {}
        if self.comment is not None:
            for line in self.comment.splitlines():
                out.write(';; ')
                out.write(line)
                out.write('\n')
        if self.annotations:
            out.write('(! ')
        out.write('(function %s (' % Expr._quote(self.name))
        sep = ""
        for p in self.params:
            out.write(sep)
            sep = ' '
            p.write(out, bindings=bindings)
            bindings[p.var] = p
        out.write(')\n(return')
        for v in self.retvars:
            out.write('\n  ')
            v.write(out, bindings=bindings)
            bindings[v.var] = v
        out.write(')\n')
        out.write('(locals')
        for l in self.locals:
            out.write('\n  ')
            l.write(out, bindings=bindings)
            bindings[l.var] = l
        out.write(')\n')
        self.body.write(out, 0, bindings)
        out.write('\n)')
        if self.annotations:
            for t in self.annotations:
                out.write(' :%s %s' % (t[0], Expr._tosexp(t[1])))
            out.write(')')
        out.write('\n')

    def get_const(self):
        """
        Get the const expression for this function.
        """
        return ConstExpr(self.get_type(), self.name)

    def get_type(self):
        """
        Get the type of this function.
        """
        return FunctionType([p.type for p in self.params],
                            [r.type for r in self.retvars])

    def _do_check(self, l, name):
        return name in set(v.var for v in l)

    def is_param(self, name):
        """
        Return true if `name` is a name of one of the function parameters.
        """
        return self._do_check(self.params, name)

    def is_return_var(self, name):
        """
        Return true if `name` is a name of one of the function return vars.
        """
        return self._do_check(self.return_vars, name)

    def is_local(self, name):
        """
        Return true if `name` is a name of one of the function local vars.
        """        
        return self._do_check(self.locals, name)

    def is_own(self, name):
        """
        Return true if `name` is a parameter, return var or local var.
        """        
        return self.is_param(name) or self.is_local(name) \
               or self.is_return_var(name)

    def _do_get(self, l, name):
        for v in l:
            if v.var == name:
                return v
        raise KeyError(name)

    def get_param(self, name):
        """
        Get the parameter with the given name. Raise `KeyError` if not found.
        """
        return self._do_get(self.params, name)

    def get_local(self, name):
        """
        Get the local var with the given name. Raise `KeyError` if not found.
        """
        return self._do_get(self.locals, name)

    def get_return_var(self, name):
        """
        Get the return var with the given name. Raise `KeyError` if not found.
        """
        return self._do_get(self.return_vars, name)

    def get_own(self, name):
        """
        Get the parameter, local var or return var with the given name.
        Raise `KeyError` if not found.
        """
        return self._do_get(self.params + self.locals + self.return_vars, name)

    def copy(self):
        """
        Return a copy of this function.
        """
        ret = Function(self.name, copy.copy(self.params),
                       copy.copy(self.retvars), copy.copy(self.locals),
                       self.body)
        if self.annotations:
            ret.annotations = copy.copy(self.annotations)
        return ret

# end of class Function


class Program(object):
    """
    A Kratos program.
    Attributes:
    - init: an constraint (boolean expression) on global vars, or None
    - entrypoint: name of the program entry point, or None
    - comment: a comment for the program, or None
    - globals: the list of global variables (list of var expressions)
    - functions: the list of program functions
    """
    class _ListProxy(object):
        def __init__(self, val=None):
            self.value = val or []
            self.valid = False

        def append(self, elem):
            self.value.append(elem)
            self.valid = False

        def extend(self, other):
            if isinstance(other, Program._ListProxy):
                other = other.value
            self.value.extend(other)
            self.valid = False

        def insert(self, pos, elem):
            self.value.insert(pos, elem)
            self.valid = False            

        def __iter__(self):
            return iter(self.value)

        def __len__(self):
            return len(self.value)

        def __getitem__(self, elem):
            return self.value[elem]

        def __setitem__(self, elem, val):
            self.valid = False
            self.value[elem] = val

        def __add__(self, other):
            if isinstance(other, Program._ListProxy):
                other = other.value
            return Program._ListProxy(self.value + other)

        def __radd__(self, other):
            if isinstance(other, Program._ListProxy):
                other = other.value
            return Program._ListProxy(self.value + other)

        def __iadd__(self, other):
            if isinstance(other, Program._ListProxy):
                other = other.value
            self.value += other
            self.valid = False
            return self

    # end of class _ListProxy
        
    def __init__(self, globals, init, entrypoint, functions):
        self._globals = Program._ListProxy(globals)
        self._glbmap = set()
        self._functions = Program._ListProxy(functions)
        self._funmap = {}
        
        self.init = init
        self.entrypoint = entrypoint
        self.comment = None
        self.globals = globals
        self.functions = functions

    @property
    def globals(self):
        return self._globals

    @globals.setter
    def globals(self, value):
        if isinstance(value, Program._ListProxy):
            value = value.value
        self._globals = Program._ListProxy(value)

    @property
    def functions(self):
        return self._functions

    @functions.setter
    def functions(self, value):
        if isinstance(value, Program._ListProxy):
            value = value.value
        self._functions = Program._ListProxy(value)

    def __str__(self):
        """
        Get a string representation of this program in K2 format.
        """
        buf = StringIO()
        self.write(buf)
        return buf.getvalue()

    def write(self, out):
        """
        Write a K2 representation of the program to the file-like `out`.
        Parameters:
        - out: a file-like object
        """
        if self.comment is not None:
            for line in self.comment.splitlines():
                out.write(';; ')
                out.write(line)
                out.write('\n')

        bindings = {}
        def add_binding(g):
            if isinstance(g.type, EnumType) and g.type not in bindings:
                b = '|enum.%d|' % len(bindings)
                bindings[g.type] = b
                out.write('(type %s ' % b)
                g.type.write(out)
                out.write(')\n')
            
        for g in self._globals.value:
            add_binding(g)
        for f in self._functions.value:
            for v in f.params:
                add_binding(v)
            for v in f.retvars:
                add_binding(v)
            for v in f.locals:
                add_binding(v)
            
        if bindings:
            out.write('\n')
        out.write('(globals')
        for g in self._globals.value:
            out.write('\n')
            g.write(out, bindings=bindings)
            bindings[g.var] = g
        out.write(')\n\n')
        if self.init:
            out.write('(init\n')
            self.init.write(out, bindings=bindings)
            out.write(')\n\n')
        if self.entrypoint:
            out.write('(entry %s)\n' % Expr._quote(self.entrypoint))
        for f in self._functions.value:
            bindings[f.name] = f.get_const()
            out.write('\n')
            f.write(out, bindings)
            out.write('\n')

    def is_global(self, name):
        """
        Returns true if `name` is the name of a global var of this program.
        """
        if not self._globals.valid:
            self._glbmap = {v.var : v for v in self._globals}
            self._globals.valid = True
        return name in self._glbmap

    def get_global(self, name):
        """
        Get the global var with the given name. Raise `KeyError` if not found.
        """
        if not self._globals.valid:
            self._glbmap = {v.var : v for v in self._globals}
            self._globals.valid = True
        return self._glbmap[name]

    def get_function(self, name):
        """
        Get the function with the given name. Raise `KeyError` if not found.
        """
        if not self._functions.valid:
            self._funmap = {f.name : f for f in self._functions}
            self._functions.valid = True
        return self._funmap[name]

    def copy(self):
        """
        Return a copy of this program.
        """
        res = Program([copy.copy(v) for v in self.globals],
                      copy.copy(self.init),
                      copy.copy(self.entrypoint),
                      [f.copy() for f in self.functions])
        res.comment = self.comment
        return res
        
# end of class Program


class ExecutionStep(object):
    """
    A step of a Kratos execution trace.
    Arguments:
    - is_path: true if this step is a subpath
    - stmt: the expression corresponding to this step (when `is_path` is false)
    - path: the subpath associated to this step (when `is_path` is true)
    """
    def __init__(self, stmt_or_path):
        self.is_path = not isinstance(stmt_or_path, Expr)
        if self.is_path:
            self.path = stmt_or_path
        else:
            self.stmt = stmt_or_path

    def write(self, out, indent=0):
        """
        Write a K2 representation of the execution step to the file-like `out`.
        Parameters:
        - out: a file-like object
        """
        if self.is_path:
            self.path.write(out, indent+1)
        else:
            out.write('  ' * indent)
            self.stmt.write(out)
            out.write('\n')

    def copy(self):
        """
        Return a copy of this execution step.
        """        
        if self.is_path:
            return ExecutionStep(self.path.copy())
        else:
            return ExecutionStep(self.stmt)

    def __str__(self):
        """
        Get a string representation of this execution step in K2 format.
        """
        buf = StringIO()
        self.write(buf)
        return buf.getvalue()        

# end of class ExecutionStep


class ExecutionPath(list):
    """
    A subpath of a Kratos execution trace.
    """
    
    def write(self, out, indent=0):
        """
        Write a K2 representation of the execution path to the file-like `out`.
        Parameters:
        - out: a file-like object
        """
        for i, s in enumerate(self):
            s.write(out, max(0, indent - (1 if i == 0 else 0)))

    def copy(self):
        """
        Return a copy of this execution path.
        """        
        return ExecutionPath(s.copy() for s in self)

    def __str__(self):
        """
        Get a string representation of this execution path in K2 format.
        """
        buf = StringIO()
        self.write(buf)
        return buf.getvalue()

# end of class ExecutionPath
