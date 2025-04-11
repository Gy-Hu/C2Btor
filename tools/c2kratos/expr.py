#
# Specialized Type and Expr subclasses for handling C
#

from kratos import *
from itertools import zip_longest
from collections import OrderedDict as odict

class RecordType(Type):
    __slots__ = ['name', 'fields', 'arrdims', 'is_union']
    
    def __init__(self, name, fields, arrdims=None, is_union=False):
        self.name = name
        self.fields = odict(fields) # ordered dict (field_name, type)
        self.arrdims = arrdims
        self.is_union = is_union

    def __eq__(self, other):
        return isinstance(other, RecordType) and self.name == other.name \
               and all(a == b for (a, b) in zip_longest(self.fields,
                                                        other.fields)) \
               and self.is_union == other.is_union
    
    def __hash__(self):
        return super(RecordType, self).__hash__() ^ hash(self.name)

    def write(self, out, bindings=None):
        if bindings and self in bindings:
            out.write(bindings[self])
        else:
            out.write('(record |%s|' % self.name)
            for f in self.fields:
                out.write(' (|%s| ' % f)
                tp = self.fields[f]
                tp.write(out, bindings)
                out.write(')')
            if self.is_union:
                out.write(' true')
            out.write(')')

# end of class RecordType


class PointerType(Type):
    __slots__ = ['dereference']
    
    def __init__(self, dereference):
        self.dereference = dereference

    def __eq__(self, other):
        return isinstance(other, PointerType) \
               and self.dereference == other.dereference

    def __hash__(self):
        return super(PointerType, self).__hash__() ^ hash(self.dereference)

    def write(self, out, bindings=None):
        if bindings and self in bindings:
            out.write(bindings[self])
        else:
            out.write('(ptr ')
            self.dereference.write(out, bindings)
            out.write(')')

# end of class PointerType


class ArrayType(Type):
    __slots__ = ['element', 'size']
    
    def __init__(self, elem, size):
        self.element = elem
        self.size = size

    def __eq__(self, other):
        return isinstance(other, ArrayType) \
               and self.element == other.element \
               and self.size == other.size

    def __hash__(self):
        return super(ArrayType, self).__hash__() ^ hash(self.element) \
               ^ hash(self.size)

    def write(self, out, bindings=None):
        if bindings and self in bindings:
            out.write(bindings[self])
        else:
            out.write('(array ')
            self.element.write(out, bindings)
            if self.size is not None:
                out.write(' %d)' % self.size)
            else:
                out.write(')')

# end of class ArrayType


class EllipsisType(Type):
    def write(self, out, bindings=None):
        out.write('ellipsis')

# end of class EllipsisType


class DerefExpr(Expr):
    __slots__ = ['lval']
    
    def __init__(self, lval):
        tp = lval.type.dereference
        super(DerefExpr, self).__init__(tp)
        self.lval = lval

    @Expr._annotate
    def write(self, out, indent=None, bindings=None):
        if indent is not None:
            out.write('  ' * indent)
        out.write('(deref ')
        self.lval.write(out, bindings=bindings)
        out.write(')')

    def children(self):
        return [self.lval]

    def equals(self, other):
        return isinstance(other, DerefExpr) \
               and self.lval.equals(other.lval)

# end of class DerefExpr


class AddrOfExpr(Expr):
    __slots__ = ['lval']
    
    def __init__(self, lval):
        tp = PointerType(lval.type)
        super(AddrOfExpr, self).__init__(tp)
        self.lval = lval

    @Expr._annotate
    def write(self, out, indent=None, bindings=None):
        if indent is not None:
            out.write('  ' * indent)
        out.write('(addrof ')
        self.lval.write(out, bindings=bindings)
        out.write(')')

    def children(self):
        return [self.lval]

    def equals(self, other):
        return isinstance(other, AddrOfExpr) \
               and self.lval.equals(other.lval)
    
# end of class AddrOfExpr


class FieldExpr(Expr):
    __slots__ = ['record', 'fieldpath']
    
    def __init__(self, record, *fieldpath):
        tp = record.type
        for f in fieldpath:
            tp = tp.fields[f]
        super(FieldExpr, self).__init__(tp)
        if isinstance(record, FieldExpr):
            self.record = record.record
            self.fieldpath = record.fieldpath + fieldpath
        else:
            self.record = record
            self.fieldpath = fieldpath
        # fieldpath is a sequence of field names

    @Expr._annotate
    def write(self, out, indent=None, bindings=None):
        if indent is not None:
            out.write('  ' * indent)
        out.write('(field ')
        self.record.write(out, bindings=bindings)
        out.write(' %s)' % ' '.join('|%s|' % f for f in self.fieldpath))

    def children(self):
        return [self.record]

    def equals(self, other):
        return isinstance(other, FieldExpr) \
               and self.record.equals(other.record) \
               and self.fieldpath == other.fieldpath

# end of class FieldExpr


class MapAtExpr(Expr):
    __slots__ = ['map', 'index']
    
    def __init__(self, map, index):
        super(MapAtExpr, self).__init__(map.type.element)
        self.map = map
        self.index = index

    @Expr._annotate
    def write(self, out, indent=None, bindings=None):
        if indent is not None:
            out.write('  ' * indent)
        out.write('(mapat ')
        self.map.write(out, bindings=bindings)
        out.write(' ')
        self.index.write(out, bindings=bindings)
        out.write(')')

    def children(self):
        return [self.map, self.index]

    def equals(self, other):
        return isinstance(other, MapAtExpr) \
               and self.map.equals(other.map) \
               and self.index.equals(other.index)

# end of class MapAtExpr


class ArrAtExpr(Expr):
    __slots__ = ['array', 'index']
    
    def __init__(self, array, index):
        super(ArrAtExpr, self).__init__(array.type.element)
        self.array = array
        self.index = index

    @Expr._annotate
    def write(self, out, indent=None, bindings=None):
        if indent is not None:
            out.write('  ' * indent)
        out.write('(arrat ')
        self.array.write(out, bindings=bindings)
        out.write(' ')
        self.index.write(out, bindings=bindings)
        out.write(')')

    def children(self):
        return [self.array, self.index]

    def equals(self, other):
        return isinstance(other, ArrAtExpr) \
               and self.array.equals(other.array) \
               and self.index.equals(other.index)

# end of class ArrAtExpr


class CompExpr(Expr):
    __slots__ = ['args']
    
    def __init__(self, *args):
        super(CompExpr, self).__init__(args[-1].type)
        self.args = list(args)

    @Expr._annotate
    def write(self, out, indent=None, bindings=None):
        if indent is not None:
            out.write('  ' * indent)
            indent += 1
        out.write('(comp')
        for a in self.args:
            out.write('\n')
            a.write(out, indent, bindings)
        out.write(')')

    def children(self):
        return self.args

    def equals(self, other):
        return isinstance(other, CompExpr) \
               and len(self.args) == len(other.args) \
               and all(a.equals(b) for (a, b) in zip(self.args, other.args))

# end of class CompExpr


class ForExpr(Expr):
    __slots__ = ['init', 'guard', 'incr', 'body']
    
    def __init__(self, init, guard, incr, body):
        super(ForExpr, self).__init__(VoidType())
        self.init = init
        self.guard = guard
        self.incr = incr
        self.body = body

    def children(self):
        return [self.init, self.guard, self.incr, self.body]

    def equals(self, other):
        return isinstance(other, ForExpr) \
               and self.init.equals(other.init) \
               and self.guard.equals(other.guard) \
               and self.incr.equals(other.incr) \
               and self.body.equals(other.body)

    @Expr._annotate
    def write(self, out, indent=None, bindings=None):
        if indent is not None:
            out.write('  ' * indent)
        out.write('(for ')
        self.init.write(out, bindings=bindings)
        out.write(' ')
        self.guard.write(out, bindings=bindings)
        out.write(' ')
        self.incr.write(out, bindings=bindings)
        if indent is not None:
            out.write('\n')
            self.body.write(out, indent+1, bindings)
        else:
            out.write(' ')
            self.body.write(out, bindings=bindings)
        out.write(')')        

# end of class ForExpr


class IdentityVisitor(IdentityVisitor):
    def visit_DerefExpr(self, e):
        return DerefExpr(self.visit(e.lval))

    def visit_AddrOfExpr(self, e):
        return AddrOfExpr(self.visit(e.lval))

    def visit_FieldExpr(self, e):
        return FieldExpr(self.visit(e.record), *e.fieldpath)

    def visit_ArrAtExpr(self, e):
        return ArrAtExpr(self.visit(e.array), self.visit(e.index))

    def visit_CompExpr(self, e):
        return CompExpr(*[self.visit(a) for a in e.args])

    def visit_ForExpr(self, e):
        return ForExpr(self.visit(e.init), self.visit(e.guard),
                       self.visit(e.incr), self.visit(e.body))

    def visit_MapAtExpr(self, e):
        return MapAtExpr(self.visit(e.map), self.visit(e.index))
    
# end of class IdentityVisitor


class InplaceIdentityVisitor(InplaceIdentityVisitor):
    def visit_DerefExpr(self, e):
        e.lval = self.visit(e.lval)
        return e

    def visit_AddrOfExpr(self, e):
        e.lval = self.visit(e.lval)
        return e

    def visit_FieldExpr(self, e):
        e.record = self.visit(e.record)
        return e

    def visit_ArrAtExpr(self, e):
        e.array = self.visit(e.array)
        e.index = self.visit(e.index)
        return e

    def visit_CompExpr(self, e):
        for i, a in enumerate(e.args):
            e.args[i] = self.visit(a)
        return e

    def visit_ForExpr(self, e):
        e.init = self.visit(e.init)
        e.guard = self.visit(e.guard)
        e.incr = self.visit(e.incr)
        e.body = self.visit(e.body)
        return e
    
    def visit_MapAtExpr(self, e):
        e.map = self.visit(e.map)
        e.index = self.visit(e.index)
        return e

# end of class InplaceIdentityVisitor
