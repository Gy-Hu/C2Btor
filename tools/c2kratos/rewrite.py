#
# Rewriting from C-Expr to normal Expr
#

from __future__ import print_function
from kratos import *
from .expr import *
import itertools
import sys
from collections import namedtuple, OrderedDict, defaultdict
import time
import copy
import math
import gc


def remove_unreachable(program, orig_entry_point):
    class ReachedVisitor(ExprVisitor):
        def __init__(self, reached, tovisit):
            self.reached = reached
            self.tovisit = tovisit

        def visit_ConstExpr(self, e):
            if isinstance(e.type, FunctionType):
                if e.value not in self.reached:
                    self.reached.add(e.value)
                    self.tovisit.append(e.value)

        def visit_CallExpr(self, e):
            if isinstance(e.func, ConstExpr):
                if e.func.value not in self.reached:
                    self.reached.add(e.func.value)
                    self.tovisit.append(e.func.value)
            elif isinstance(e.func, CompExpr):
                self.visit(e.func)
            else:
                for fn in program.functions:
                    if fn.get_type() == e.func.type:
                        if fn.name not in self.reached:
                            self.reached.add(fn.name)
                            self.tovisit.append(fn.name)

            for arg in e.args:
                self.visit(arg)

            for ret in e.ret:
                self.visit(ret)

    # end of class ReachedVisitor

    reached = set([orig_entry_point])
    funcs = dict((fn.name, fn) for fn in program.functions)
    tovisit = [orig_entry_point]
    while tovisit:
        name = tovisit[-1]
        f = funcs.get(name)
        tovisit.pop()
        if f is not None:
            v = ReachedVisitor(reached, tovisit)
            v.visit(f.body)

    reachedfuncs = [fn for fn in program.functions if fn.name in reached]

    # remove unreachable global vars. This assumes that the entry point is
    # `init.and.main and that the only statements in it are initialization of
    # globals and a call to the "real" entry point

    used = set()
    glbls = set(v.var for v in program.globals)

    class UsedVarVisitor(ExprVisitor):
        def visit_VarExpr(self, e):
            if e.var in glbls:
                used.add(e.var)

    for fn in reachedfuncs:
        v = UsedVarVisitor()
        v.visit(fn.body)

    class AddressTakenVarVisitor(ExprVisitor):
        def visit_AddrOfExpr(self, e):
            v = UsedVarVisitor()
            v.visit(e.lval)

    # also add globals whose address has been taken during initialization
    atv = AddressTakenVarVisitor()
    entry = program.get_function(program.entrypoint)
    atv.visit(entry.body)

    class UsedFinder(ExprVisitor):
        def __init__(self):
            self.seen = False

        def visit_ArrAtExpr(self, e):
            self.visit(e.array)

        def visit_MapAtExpr(self, e):
            self.visit(e.map)

        def visit_VarExpr(self, e):
            if e.var in used or e.var not in glbls:
                self.seen = True

    class UnusedAssignRemover(IdentityVisitor):
        def __init__(self):
            self.finder = UsedFinder()

        def visit_AssignExpr(self, e):
            self.finder.seen = False
            self.finder.visit(e.lhs)
            if self.finder.seen:
                return e
            else:
                return None

        def visit_SeqExpr(self, e):
            seq = []
            for a in e.args:
                aa = self.visit(a)
                if aa is not None:
                    seq.append(aa)
            n = len(seq)
            if n == 1:
                return seq[0]
            elif n > 1:
                return SeqExpr(*seq)
            else:
                return AssumeExpr(ConstExpr(True))

        def visit_CompExpr(self, e):
            seq = []
            for a in e.args:
                aa = self.visit(a)
                if aa is not None:
                    seq.append(aa)
            assert seq
            if len(seq) == 1:
                return seq[0]
            else:
                return CompExpr(*seq)

    # end of class UnusedAssignRemover

    program.functions = reachedfuncs
    entry = funcs[program.entrypoint]
    rem = UnusedAssignRemover()
    body = rem.visit(entry.body)
    if body is None:
        body = AssumeExpr(ConstExpr(True))
    entry.body = body
    try:
        program.get_function(entry.name)
    except KeyError:
        program.functions.append(entry)
    program.globals = [v for v in program.globals if v.var in used]

    return program


def transform_program(program, Transformer):
    if program.init:
        ff = Transformer(program.globals)
        program.init = ff.visit(program.init)
        program.globals += ff.vp.added

    for fn in program.functions:
        ff = Transformer(program.globals, fn.params, fn.retvars, fn.locals)
        fn.body = ff.visit(fn.body)
        fn.locals += ff.vp.added

    return program


class ForRemover(IdentityVisitor):
    def __init__(self, opts, *scopes):
        self.use_condjump = opts and opts.use_condjump
        self.vp = VarProvider(*scopes)
        self.annotate_labels = opts and opts.annotate_labels

    def visit_ForExpr(self, e):
        def lbl(name):
            v = self.vp.mkvar(LabelType(), 'for#%s' % name)
            self.vp.added.pop()
            ret = LabelExpr(v.var)
            if self.annotate_labels:
                ret.annotations = [('label-type', 'loop-for-' + name)]
            return ret
        def ann(e, key):
            if e.annotations:
                e.annotations.append((key, ''))
            else:
                e.annotations = [(key, '')]
            return e
        looptest = lbl('test')
        loopentry = lbl('entry')
        loopexit = lbl('exit')
        guard = self.visit(e.guard)
        incr = self.visit(e.incr)
        if isinstance(incr, CompExpr) and len(incr.args) == 2 \
           and isinstance(incr.args[0], AssignExpr) and \
           incr.args[0].lhs.equals(incr.args[1]):
            incr = incr.args[0]
        incr = ann(incr, 'for-incr')
        if self.use_condjump:
            seq = [
                ann(self.visit(e.init), 'for-init'),
                looptest,
                ann(CondJumpExpr(guard, loopentry), 'for-guard'),
                JumpExpr(loopexit),
                loopentry,
                ann(self.visit(e.body), 'for-body'),
                incr,
                JumpExpr(looptest),
                loopexit,                
                ]
        else:
            # for (init; guard; incr) { body }  ==>
            #     init;
            #     test: jump(entry, exit);
            #     entry: assume(guard); body; jump(test);
            #     exit: assume(!guard);
            incr = ann(self.visit(e.incr), 'for-incr')
            seq = [
                ann(self.visit(e.init), 'for-init'),
                looptest,
                JumpExpr(loopentry, loopexit),
                loopentry,
                ann(AssumeExpr(guard), 'for-guard'),
                ann(self.visit(e.body), 'for-body'),
                incr,
                JumpExpr(looptest),
                loopexit,
                AssumeExpr(OpExpr(OpExpr.NOT, guard))
                ]
        return ann(SeqExpr(*seq), 'for-loop')

    def visit_SeqExpr(self, e):
        args = [self.visit(a) for a in e.args]
        if len(args) == 1:
            return args[0]
        return SeqExpr(*args)

# end of class ForRemover


class CompRemover(IdentityVisitor):
    def __init__(self, *scopes):
        self.vp = VarProvider(*scopes)
        self.seq = []
        self.added = set()

    def mk_seq(self, e, ann=None):
        if not self.seq:
            e.annotations = ann
            return e
        s = [a for a in (self.seq + [e]) if
             isinstance(a.type, (VoidType, LabelType))]
        if s:
            ret = SeqExpr(*s)
        else:
            ret = AssumeExpr(ConstExpr(True))
        ret.annotations = ann
        self.seq = []
        return ret

    def visit_CompExpr(self, e):
        args = [self.visit(c) for c in e.args]
        r = args[-1]
        args.pop()
        v = self.vp.mkvar(e.type)
        self.seq += args
        self.seq.append(AssignExpr(v, r))
        v.annotations = copy.copy(e.annotations)
        self.added.add(id(v))
        return v

    def visit_AssumeExpr(self, e):
        cond = self.visit(e.cond)
        a = AssumeExpr(cond)
        ann = None
        if id(cond) in self.added:
            ann = cond.annotations
            cond.annotations = None
        return self.mk_seq(a, ann)

    def visit_SeqExpr(self, e):
        args = []
        for a in e.args:
            aa = self.visit(a)
            if isinstance(a, CompExpr):
                args += [s for s in self.seq if
                         isinstance(s.type, (VoidType, LabelType))]
                self.seq = []
            else: #if isinstance(aa.type, (VoidType, LabelType)):
                args.append(self.mk_seq(aa, a.annotations))
        return SeqExpr(*args)

    def visit_CallExpr(self, e):
        return self.mk_seq(CallExpr(self.visit(e.func),
                                    [self.visit(a) for a in e.args],
                                    [self.visit(r) for r in e.ret]),
                           e.annotations)

    def visit_AssignExpr(self, e):
        r = self.visit(e.rhs)
        l = self.visit(e.lhs)
        return self.mk_seq(AssignExpr(l, r), e.annotations)

    def visit_HavocExpr(self, e):
        l = self.visit(e.lval)
        if not isinstance(l, VarExpr):
            v = self.vp.auxvar(l.type)
            self.seq.append(HavocExpr(v))
            return self.mk_seq(AssignExpr(l, v), e.annotations)
        else:
            return self.mk_seq(HavocExpr(l), e.annotations)

    def visit_CondJumpExpr(self, e):
        c = self.visit(e.cond)
        t = self.visit(e.target)
        return self.mk_seq(CondJumpExpr(c, t), e.annotations)

    def visit_TypeCastExpr(self, e):
        expr = self.visit(e.expr)
        if isinstance(e.type, VoidType):
            return AssumeExpr(ConstExpr(True))
        else:
            return TypeCastExpr(e.type, expr, e.is_bitcast)

# end of class CompRemover


def remove_side_effects(program, opts=None):
    use_condjump = opts and opts.use_condjump
    def to_ptr_type(tp):
        if isinstance(tp, ArrayType):
            return PointerType(to_ptr_type(tp.element))
        else:
            return tp

    class ArrayFunArgRemover(IdentityVisitor):
        def __init__(self, arrargs):
            self.arrargs = arrargs

        def visit_VarExpr(self, e):
            if e.var in self.arrargs:
                return VarExpr(to_ptr_type(e.type), e.var)
            else:
                return e

        def visit_ArrAtExpr(self, e):
            a = self.visit(e.array)
            i = self.visit(e.index)
            if isinstance(a.type, PointerType):
                if a.type != i.type:
                    i = TypeCastExpr(a.type, i, True)
                return DerefExpr(OpExpr(OpExpr.ADD, a, i))
            else:
                return ArrAtExpr(a, i)

    # end of class ArrayFunArgRemover

    program = transform_program(program, lambda *args: ForRemover(opts, *args))
    program = transform_program(program, CompRemover)

    for fn in program.functions:
        arrargs = set()
        for i, a in enumerate(fn.params):
            if isinstance(a.type, ArrayType):
                fn.params[i] = VarExpr(to_ptr_type(a.type), a.var)
                arrargs.add(a.var)
        for i, a in enumerate(fn.retvars):
            if isinstance(a.type, ArrayType):
                fn.retvars[i] = VarExpr(to_ptr_type(a.type), a.var)
                arrargs.add(a.var)
        if arrargs:
            v = ArrayFunArgRemover(arrargs)
            fn.body = v.visit(fn.body)

    return program


def flatten_fields(program):
    class FieldFlattener(IdentityVisitor):
        def __init__(self, *scopes):
            self.vp = VarProvider(*scopes)
            self.seq = []

        def visit_FieldExpr(self, e):
            r = e
            path = []
            while isinstance(r, FieldExpr):
                path += r.fieldpath
                r = r.record
            r = self.visit(r)
            asgn = None
            good = False
            if isinstance(r, DerefExpr):
                if isinstance(r.lval, VarExpr):
                    good = True
                else:
                    v = self.vp.auxvar(r.lval.type)
                    asgn = AssignExpr(v, r.lval)
                    r = DerefExpr(v)
            elif isinstance(r, ArrAtExpr):
                v = self.vp.auxvar(PointerType(r.type))
                asgn = AssignExpr(v, AddrOfExpr(r))
                r = DerefExpr(v)
            elif isinstance(r, MapAtExpr):
                pass
            elif not good and not isinstance(r, VarExpr):
                v = self.vp.auxvar(r.type)
                asgn = AssignExpr(v, r)
                r = v
            ee = FieldExpr(r, *path)
            if asgn is not None:
                self.seq.append(asgn)
                #ee = SeqExpr(asgn, ee)
            return ee

        def visit_SeqExpr(self, e):
            seq = []
            for a in e.args:
                self.seq = []
                aa = self.visit(a)
                if self.seq:
                    seq += self.seq
                seq.append(aa)
            self.seq = []
            return SeqExpr(*seq)

        def _mkseq(self, ee):
            if self.seq:
                self.seq.append(ee)
                ee = SeqExpr(*self.seq)
                self.seq = []
            return ee

        def visit_AssignExpr(self, e):
            r = self.visit(e.rhs)
            l = self.visit(e.lhs)
            ee = AssignExpr(l, r)
            return self._mkseq(ee)

        def visit_AssumeExpr(self, e):
            ee = self.visit(e.cond)
            return self._mkseq(AssumeExpr(ee))

        def visit_CallExpr(self, e):
            f = self.visit(e.func)
            args = [self.visit(a) for a in e.args]
            ret = [self.visit(r) for r in e.ret]
            return self._mkseq(CallExpr(f, args, ret))

        def visit_HavocExpr(self, e):
            ee = self.visit(e.lval)
            return self._mkseq(HavocExpr(ee))

    # end of class FieldFlattener

    return transform_program(program, FieldFlattener)


class ArrayCollector(ExprVisitor):
    def __init__(self, maxlen=None, allow_var_idx=True):
        self.maxlen = maxlen
        self.allow_var_idx = allow_var_idx
        self.touched = set()
        self.in_addr = [False]

    def visit_VarExpr(self, e):
        if isinstance(e.type, ArrayType):
            self.touched.add(str(e))

    def visit_FieldExpr(self, e):
        if isinstance(e.type, ArrayType) \
           and isinstance(e.record, VarExpr):
            self.touched.add(str(e))
        self.visit(e.record)

    def visit_AddrOfExpr(self, e):
        def has_arrays(tp):
            for f in tp.fields:
                t = tp.fields[f]
                if isinstance(t, ArrayType):
                    return True
                elif isinstance(t, RecordType):
                    return has_arrays(t)
            return False

        if isinstance(e.lval.type, RecordType) and has_arrays(e.lval.type):
            self.touched.add(str(e.lval))
        self.in_addr.append(True)
        self.visit(e.lval)
        self.in_addr.pop()

    def visit_ArrAtExpr(self, e):
        self.visit(e.index)
        stop = False
        if not self.in_addr[-1]:
            if not self.allow_var_idx and not isinstance(e.index, ConstExpr):
                stop = False
            elif isinstance(e.array, VarExpr):
                stop = self.maxlen is None or \
                       (e.array.type.size <= self.maxlen)
            elif isinstance(e.array, FieldExpr) \
                 and isinstance(e.array.record, VarExpr):
                stop = self.maxlen is None or (e.array.type.size <= self.maxlen)
        if not stop:
            if isinstance(e.array, ArrAtExpr):
                self.visit(e.array)
            else:
                self.in_addr.append(True)
                self.visit(e.array)
                self.in_addr.pop()

    def can_blast(self, arr):
        if not isinstance(arr.type, ArrayType):
            return False
        if isinstance(arr, VarExpr):
            return str(arr) not in self.touched
        elif isinstance(arr, FieldExpr) and isinstance(arr.record, VarExpr):
            return str(arr) not in self.touched \
                   and str(arr.record) not in self.touched
        else:
            return False

# end of class ArrayCollector


class ArrayBlaster(IdentityVisitor):
    def __init__(self, info, blastmap, *scopes, **kwds):
        self.info = info
        self.blastmap = blastmap
        self.vp = VarProvider(*scopes)
        self.has_comp = False
        self.glbls = set(v.var for v in scopes[0])
        self.globals = []
        self.locals = []
        self._aux = set()
        opts = kwds.get('opts')
        self.use_condjump = opts and opts.use_condjump
        self.annotate_labels = opts and opts.annotate_labels

    def visit_ArrAtExpr(self, e):
        if self.info.can_blast(e.array):
            return self._make_arrat(self.visit(e.array),
                                    self.visit(e.index))
        return ArrAtExpr(self.visit(e.array), self.visit(e.index))

    def visit_FieldExpr(self, e):
        if isinstance(e.record, ArrAtExpr) \
               and self.info.can_blast(e.record.array):
            return self._make_arrat(self.visit(e.record.array),
                                    self.visit(e.record.index),
                                    fieldpath=e.fieldpath)
        return super(ArrayBlaster, self).visit_FieldExpr(e)

    def visit_AssignExpr(self, e):
        rhs = self.visit(e.rhs)
        if isinstance(e.lhs, ArrAtExpr) \
               and self.info.can_blast(e.lhs.array):
            return self._make_arrat(self.visit(e.lhs.array),
                                    self.visit(e.lhs.index), rhs=rhs)
        elif isinstance(e.lhs, FieldExpr) \
             and isinstance(e.lhs.record, ArrAtExpr) \
             and self.info.can_blast(e.lhs.record.array):
            return self._make_arrat(self.visit(e.lhs.record.array),
                                    self.visit(e.lhs.record.index), rhs=rhs,
                                    fieldpath=e.lhs.fieldpath)
        return super(ArrayBlaster, self).visit_AssignExpr(e)

    def _label(self, name, purpose):
        res = LabelExpr(name)
        if self.annotate_labels:
            res.annotations = [('label-type', purpose)]
        return res

    def _make_arrat(self, arr, index, fieldpath=None, rhs=None):
        key = str(arr)
        if isinstance(arr, VarExpr):
            prefix = arr.var
            is_global = arr.var in self.glbls
        elif isinstance(arr, FieldExpr):
            prefix = '%s->%s' % (arr.record.var,
                                 '->'.join(arr.fieldpath))
            is_global = arr.record.var in self.glbls
        l = self.blastmap.get(key)
        if l is None:
            l = []
            for i in range(arr.type.size):
                v = self.vp.mkvar(arr.type.element, '%s[%d]' % (prefix, i))
                l.append(v)
                if is_global:
                    self.globals.append(v)
                else:
                    self.locals.append(v)
            self.blastmap[key] = l
        idx = self.eval_index(index)
        if idx is None or idx < 0 or idx >= arr.type.size:
            lbls = []
            for v in l:
                name = self.vp.mkvar(LabelType(), '%case.' + v.var).var
                lbls.append(self._label(name, 'switch-case'))
            name = self.vp.mkvar(LabelType(), '%end.' + prefix).var
            end = self._label(name, 'switch-case')
            res = None
            if rhs is None:
                res = self.vp.auxvar(arr.type.element)
                if res.var not in self._aux:
                    self._aux.add(res.var)
                    self.locals.append(res)
            conds = []
            for i, v in enumerate(l):
                c = OpExpr(OpExpr.EQUAL, index,
                           ConstExpr(index.type, str(i)))
                conds.append(c)
            if self.use_condjump:
                seq = []
                for i, v in enumerate(l):
                    seq.append(CondJumpExpr(conds[i], lbls[i]))
                seq.append(JumpExpr(end))
            else:
                seq = [JumpExpr(*lbls)]                
            for i, v in enumerate(l):
                seq.append(lbls[i])
                if not self.use_condjump:
                    seq.append(AssumeExpr(conds[i]))
                if res is not None:
                    seq.append(AssignExpr(res, v))
                elif fieldpath is not None:
                    seq.append(AssignExpr(FieldExpr(v, *fieldpath), rhs))
                else:
                    seq.append(AssignExpr(v, rhs))
                seq.append(JumpExpr(end))
            seq.append(end)
            if res is not None:
                if fieldpath is not None:
                    res = FieldExpr(res, *fieldpath)
                seq.append(res)
                self.has_comp = True
                return CompExpr(*seq)
            else:
                return SeqExpr(*seq)
        else:
            res = l[idx]
            if fieldpath is not None:
                res = FieldExpr(res, *fieldpath)
            if rhs is not None:
                res = AssignExpr(res, rhs)
            return res

    def eval_index(self, index):
        while isinstance(index, TypeCastExpr) and not index.is_bitcast \
              and isinstance(index.type, (IntType, BVType)):
            index = index.expr # WARNING: ignoring truncations here...
        if isinstance(index, ConstExpr) \
               and isinstance(index.type, (IntType, BVType)):
            try:
                return int(index.value)
            except ValueError:
                pass
        return None

# end of class ArrayBlaster


def blast_static_arrays(program, maxlen, allow_var_idx, opts=None):
    info = ArrayCollector(maxlen, allow_var_idx)
    if program.init:
        info.visit(program.init)
    for fn in program.functions:
        for p in fn.params:
            info.visit(p)
        info.visit(fn.body)
        
    new_globals = [v for v in program.globals if not info.can_blast(v)]
    blastmap = {}
    if program.init:
        b = ArrayBlaster(info, blastmap, program.globals, opts=opts)
        program.init = b.visit(program.init)
        program.globals += b.globals
        program.globals += b.locals
        ## if b.has_comp:
        ##     cr = CompRemover(program.globals)
        ##     program.init = cr.visit(program.init)
        ##     new_globals += cr.vp.added

    for fn in program.functions:
        b = ArrayBlaster(info, blastmap, program.globals, fn.locals, fn.params,
                         fn.retvars, opts=opts)
        fn.body = b.visit(fn.body)
        fn.locals = [v for v in fn.locals if not info.can_blast(v)]
        fn.locals += b.locals
        new_globals += b.globals
        ## if b.has_comp:
        ##     cr = CompRemover(program.globals, fn.locals, fn.params, fn.retvars)
        ##     fn.body = cr.visit(fn.body)
        ##     fn.locals += cr.vp.added

    program.globals = new_globals

    return program


def remove_static_arrays(program):
    info = ArrayCollector()
    if program.init:
        info.visit(program.init)
    for fn in program.functions:
        for p in fn.params:
            info.visit(p)
        info.visit(fn.body)

    class ArrayRemover(IdentityVisitor):
        def __init__(self, arrmap, info, *scopes):
            self.arrmap = arrmap
            self.info = info
            self.vp = VarProvider(*scopes)
            self.glbls = set(v.var for v in scopes[0])
            self.globals = []
            self.locals = []

        def visit_ArrAtExpr(self, e):
            if self.info.can_blast(e.array):
                return self._make_arrat(self.visit(e.array),
                                        self.visit(e.index))
            a = self.visit(e.array)
            i = self.visit(e.index)
            if isinstance(a, MapAtExpr):
                return MapAtExpr(a, i)
            return ArrAtExpr(a, i)

        def visit_FieldExpr(self, e):
            if isinstance(e.record, ArrAtExpr) \
                   and self.info.can_blast(e.record.array):
                return self._make_arrat(self.visit(e.record.array),
                                        self.visit(e.record.index),
                                        fieldpath=e.fieldpath)
            return super(ArrayRemover, self).visit_FieldExpr(e)

        def visit_AssignExpr(self, e):
            rhs = self.visit(e.rhs)
            if isinstance(e.lhs, ArrAtExpr) \
                   and self.info.can_blast(e.lhs.array):
                return self._make_arrat(self.visit(e.lhs.array),
                                        self.visit(e.lhs.index), rhs=rhs)
            elif isinstance(e.lhs, FieldExpr) \
                 and isinstance(e.lhs.record, ArrAtExpr) \
                 and self.info.can_blast(e.lhs.record.array):
                return self._make_arrat(self.visit(e.lhs.record.array),
                                        self.visit(e.lhs.record.index), rhs=rhs,
                                        fieldpath=e.lhs.fieldpath)
            return super(ArrayRemover, self).visit_AssignExpr(e)

        def _map_type(self, idx, elem):
            if isinstance(elem, ArrayType):
                elem = self._map_type(idx, elem.element)
            return MapType(idx, elem)

        def _make_arrat(self, arr, index, fieldpath=None, rhs=None):
            key = str(arr)
            if isinstance(arr, VarExpr):
                prefix = arr.var
                is_global = arr.var in self.glbls
            elif isinstance(arr, FieldExpr):
                prefix = '%s->%s' % (arr.record.var,
                                     '->'.join(arr.fieldpath))
                is_global = arr.record.var in self.glbls
            l = self.arrmap.get(key)
            if l is None:
                #l = self.vp.mkvar(MapType(index.type, arr.type.element), prefix)
                l = self.vp.mkvar(self._map_type(index.type, arr.type.element),
                                  prefix)
                if is_global:
                    self.globals.append(l)
                else:
                    self.locals.append(l)
                self.arrmap[key] = l
            res = MapAtExpr(l, index)
            if fieldpath is not None:
                res = FieldExpr(res, *fieldpath)
            if rhs is not None:
                res = AssignExpr(res, rhs)
            return res

    # end of class ArrayRemover
        
    new_globals = [v for v in program.globals if not info.can_blast(v)]
    arrmap = {}
    if program.init:
        b = ArrayRemover(arrmap, info, program.globals)
        program.init = b.visit(program.init)
        program.globals += b.globals
        program.globals += b.locals

    for fn in program.functions:
        b = ArrayRemover(arrmap, info, program.globals, fn.locals, fn.params,
                         fn.retvars)
        fn.body = b.visit(fn.body)
        fn.locals = [v for v in fn.locals if not info.can_blast(v)]
        fn.locals += b.locals
        new_globals += b.globals

    program.globals = new_globals

    return program


def get_record_leaves(r):
    if isinstance(r.type, RecordType):
        for f in r.type.fields:
            for c in get_record_leaves(FieldExpr(r, f)):
                yield c
    else:
        yield r

def get_record_leaf_types(tp):
    if isinstance(tp, RecordType):
        for t in tp.fields.values():
            for tt in get_record_leaf_types(t):
                yield tt
    else:
        yield tp


def get_union_constraints(field_lval, mem_address_type):
    def get_bits(tp):
        ret = 0
        for t in get_record_leaf_types(tp):
            if isinstance(t, ArrayType):
                ret += t.size * get_bits(t.element)
            elif isinstance(t, BVType):
                ret += t.bits
            elif isinstance(t, FPType):
                ret += t.bits
            elif isinstance(t, PointerType):
                ret += mem_address_type.bits
            else:
                return None
        return ret

    def get_bv_representation(t, tp, bw):
        assert(get_bits(tp) == bw)
        bvt = BVType(bw, 0)

        if isinstance(tp, (BVType, FPType)):
            return TypeCastExpr(bvt, t, 1)
        elif isinstance(tp, RecordType) and len(tp.fields) > 0 \
             and all(isinstance(tp.fields[f], BVType) for f in tp.fields):

            ff = list(tp.fields)

            # NOTE: The following assumes little-endian storage.
            # E.g., the second field is "more significant" than the
            # first field. The second field thus needs to be
            # multiplied by 2^n, i.e., shifted *left*.

            # initialize the result with the first field
            res_expr = TypeCastExpr(bvt, FieldExpr(t, ff[0]), 1)
            cur_shift = tp.fields[ff[0]].bits

            # bitwise-or the remaining (appropriately shifted) fields
            for f in ff[1:]:
                cur = OpExpr(OpExpr.LSHIFT,
                             TypeCastExpr(bvt, FieldExpr(t, f), 1),
                             ConstExpr(bvt, str(cur_shift)))
                res_expr = OpExpr(OpExpr.BITOR, res_expr, cur)

                cur_shift += tp.fields[f].bits

            return res_expr
        else:
            return None

    assert isinstance(field_lval, FieldExpr)
    r = field_lval.record
    tp = r.type
    utp = None
    ufpth = None
    for i, f in enumerate(field_lval.fieldpath):
        if isinstance(tp, RecordType) and tp.is_union:
            utp = tp
            ufpth = field_lval.fieldpath[:i]
        tp = tp.fields[f]
    res = []
    if utp is not None:
        bits = { f : get_bits(utp.fields[f]) for f in utp.fields }
        v = FieldExpr(r, *ufpth) if ufpth else r

        # identify the union member whose subfield was modified
        assigned_to = None
        for f in utp.fields:
            for l in get_record_leaves(FieldExpr(v, f)):
                if field_lval.equals(l):
                    assigned_to = f
                    break
            if assigned_to is not None:
                break
        assert assigned_to is not None

        # havoc all subfields of all the other members
        for f in utp.fields:
            if f == assigned_to:
                continue

            for l in get_record_leaves(FieldExpr(v, f)):
                res.append(HavocExpr(l))

        # NOTE: we only support precise type-punning between two members with
        # the same number of bits, where the members are of type BV, FP or
        # struct of BVs
        members_sizes = defaultdict(list)
        for key, tp in utp.fields.items():
            t = FieldExpr(v, key)
            bw = get_bits(tp)
            if bw is not None:
                members_sizes[bw].append((t, tp))

        for bw, members in members_sizes.items():
            # get bit-vector representations for all members of bit-width bw
            bvs = (get_bv_representation(t, tp, bw) for t, tp in members)
            bvs = [bv for bv in bvs if bv is not None]

            # assert that the bit-vector representations are equal
            for i in range(len(bvs)-1):
                a = bvs[i]
                b = bvs[i+1]
                res.append(AssumeExpr(OpExpr(OpExpr.EQUAL, a, b)))
    return res


def expand_records(program, opts):
    def get_function_type(tp):
        args = []
        ret = []
        for a in tp.args:
            if isinstance(a, RecordType):
                args += list(get_record_leaf_types(a))
            else:
                args.append(a)
        for r in tp.ret:
            if isinstance(r, RecordType):
                ret += list(get_record_leaf_types(r))
            else:
                ret.append(r)
        return FunctionType(args, ret)
            
    class RecordExpander(IdentityVisitor):
        def __init__(self):
            self.fired = False
            
        def visit_AssignExpr(self, e):
            rhs = self.visit(e.rhs)
            lhs = self.visit(e.lhs)
            if isinstance(lhs.type, RecordType):
                lleaves = get_record_leaves(lhs)
                rleaves = get_record_leaves(rhs)
                seq = []
                for (l, r) in zip(lleaves, rleaves):
                    seq.append(AssignExpr(l, r))
                self.fired = True
                ret = SeqExpr(*seq)
                return ret
            else:
                return AssignExpr(lhs, rhs)

        def visit_CallExpr(self, e):
            if not isinstance(e.func, ConstExpr):
                # just check that there are no records, otherwise bail out
                for a in e.args:
                    assert not isinstance(a.type, RecordType)
                for r in e.ret:
                    assert not isinstance(r.type, RecordType)
                return e
            
            func = ConstExpr(get_function_type(e.func.type), e.func.value)
            args = []
            for a in e.args:
                if isinstance(a.type, RecordType):
                    for f in get_record_leaves(a):
                        args.append(f)
                    self.fired = True
                else:
                    args.append(a)
            ret = []
            for r in e.ret:
                if isinstance(r.type, RecordType):
                    for f in get_record_leaves(r):
                        ret.append(f)
                    self.fired = True
                else:
                    ret.append(r)
            return CallExpr(func, args, ret)

        def visit_HavocExpr(self, e):
            seq = []
            if isinstance(e.lval.type, RecordType):
                self.fired = True
                return SeqExpr(*[HavocExpr(f) for f
                                 in get_record_leaves(e.lval)])
            return HavocExpr(self.visit(e.lval))

    # end of class RecordExpander

    memtype = IntType()
    if opts.bitvectors:
        memtype = BVType(opts.bitvectors, False)

    def check_unions(e):
        if isinstance(e.type, RecordType) and e.type.is_union:
            return True
        else:
            for c in e.children():
                if check_unions(c):
                    return True
        return False

    class UnionConstraintAdder(IdentityVisitor):
        def visit_AssignExpr(self, e):
            if isinstance(e.lhs, FieldExpr):
                c = get_union_constraints(e.lhs, memtype)
                if c:
                    return SeqExpr(e, *c)
            return e

        def visit_HavocExpr(self, e):
            if isinstance(e.lval, FieldExpr):
                c = get_union_constraints(e.lval, memtype)
                if c:
                    return SeqExpr(e, *c)
            return e

        def visit_CallExpr(self, e):
            cc = []
            for r in e.ret:
                if isinstance(r, FieldExpr):
                    c = get_union_constraints(r, memtype)
                    if c:
                        cc += c
            if cc:
                return SeqExpr(e, *cc)
            return e

    # end of class UnionConstraintAdder

    exp = RecordExpander()
    if program.init:
        program.init = exp.visit(program.init)

    for fn in program.functions:
        vp = VarProvider(program.globals, fn.params, fn.retvars, fn.locals)
        params = []
        retvars = []
        pre = []
        post = []

        for i, p in enumerate(fn.params):
            if isinstance(p.type, RecordType):
                for j, f in enumerate(get_record_leaves(p)):
                    v = vp.mkvar(f.type, '$arg.field.%d.%d' % (i, j))
                    params.append(v)
                    pre.append(AssignExpr(f, v))
                    fn.locals.append(p)
            else:
                params.append(p)
        fn.params = params

        for i, r in enumerate(fn.retvars):
            if isinstance(r.type, RecordType):
                for j, f in enumerate(get_record_leaves(r)):
                    v = vp.mkvar(f.type, '$ret.field.%d.%d' % (i, j))
                    retvars.append(v)
                    post.append(AssignExpr(v, f))
                    fn.locals.append(r)
            else:
                retvars.append(r)
        fn.retvars = retvars
        
        body = exp.visit(fn.body)
        if pre or post:
            fn.body = SeqExpr(*(pre + [body] + post))
        else:
            fn.body = body

    if exp.fired:
        program = flatten_fields(program)

    glbl_unions = any(check_unions(v) for v in program.globals)
    uc = UnionConstraintAdder()
    for f in program.functions:
        cur_unions = any(check_unions(v)
                         for v in itertools.chain(f.locals, f.params, f.retvars))
        if cur_unions or (glbl_unions and check_unions(f.body)):
            f.body = uc.visit(f.body)
            
    return program


def get_calls_map(program):
    class CallFinder(ExprVisitor):
        def __init__(self, name, calls):
            self.name = name
            self.calls = calls

        def visit_CallExpr(self, e):
            if isinstance(e.func, ConstExpr):
                self.calls.setdefault(self.name, set()).add(e.func.value)

    # end of class CallFinder

    calls = {}
    for fn in program.functions:
        cf = CallFinder(fn.name, calls)
        cf.visit(fn.body)

    return calls


def rewrite_output_params(program):
    calls = get_calls_map(program)
    seen = set()
    def toposort(n):
        if n not in seen:
            seen.add(n)
            for name in calls.get(n, []):
                toposort(name)
            yield n

    class OutParDetector(ExprVisitor):
        def __init__(self, candidates, outmap):
            self.candidates = candidates
            self.outmap = outmap
            self.seen = set()

        def visit_AssignExpr(self, e):
            self.visit(e.rhs)
            l = e.lhs
            if isinstance(l, DerefExpr) and isinstance(l.lval, VarExpr) \
                   and l.lval.var in self.candidates \
                   and not isinstance(l.type, RecordType):
                self.seen.add(l.lval.var)
            elif isinstance(l, FieldExpr) and isinstance(l.record, DerefExpr) \
                 and isinstance(l.record.lval, VarExpr) \
                 and l.record.lval.var in self.candidates:
                self.candidates[l.record.lval.var].add(
                    (tuple(l.fieldpath), l.type))
                self.seen.add(l.record.lval.var)
            else:
                self.visit(l)

        def visit_VarExpr(self, e):
            if e.var in self.candidates:
                del self.candidates[e.var]

        def visit_CallExpr(self, e):
            if isinstance(e.func, ConstExpr):
                outpars = self.outmap.get(e.func.value, set())
            else:
                outpars = set()
            self.visit(e.func)
            for r in e.ret:
                self.visit(r)
            for i, a in enumerate(e.args):
                if isinstance(a, VarExpr) and a.var in self.candidates \
                   and i in outpars:
                    pass
                else:
                    self.visit(a)

    # end of class OutParDetector

    outmap = {}
    ts = dict((n, i) for i, n in enumerate(toposort(program.entrypoint)))
    order = sorted(program.functions,
                   key=lambda f: (ts.get(f.name, -1), f.name))
    for fn in order:
        candidates = dict((p.var, set()) for p in fn.params
                          if isinstance(p.type, PointerType))
        v = OutParDetector(candidates, outmap)
        v.visit(fn.body)
        m = {}
        for i, p in enumerate(fn.params):
            if p.var in v.candidates and p.var in v.seen:
                f = v.candidates[p.var]
                if not isinstance(p.type.dereference, RecordType) or f:
                    m[i] = f
        outmap[fn.name] = m


    class OutParReplacer(IdentityVisitor):
        def __init__(self, outmap, outpvars, *scopes):
            self.outmap = outmap
            self.outpvars = outpvars
            self.vp = VarProvider(*scopes)
            self.all_auxvars = {}
            self.used_auxvars = set()

        def auxvar(self, tp):
            vl = self.all_auxvars.get(tp, [])
            for v in vl:
                if v.var not in self.used_auxvars:
                    self.used_auxvars.add(v.var)
                    return v
            v = self.vp.mkvar(tp, '$aux')
            self.all_auxvars.setdefault(tp, []).append(v)
            self.used_auxvars.add(v.var)
            return v

        def visit_CallExpr(self, e):
            if isinstance(e.func, ConstExpr) and e.func.value in self.outmap:
                self.used_auxvars = set()
                args = []
                ret = e.func.type.ret
                outpars = self.outmap[e.func.value]
                for i, a in enumerate(e.args):
                    if i in outpars:
                        f = outpars[i]
                        if f:
                            ret += [t[1] for t in sorted(f)]
                        else:
                            ret.append(a.type.dereference)
                    else:
                        args.append(a.type)
                tp = FunctionType(args, ret)
                func = ConstExpr(tp, e.func.value)
                ret = [self.visit(r) for r in e.ret]
                args = []
                seq = []

                def mk_deref(a):
                    if isinstance(a, AddrOfExpr):
                        return a.lval
                    return DerefExpr(a)
                
                for i, a in enumerate(e.args):
                    aa = self.visit(a)
                    if i in outpars:
                        f = outpars[i]
                        if f:
                            for (fieldpath, tp) in sorted(f):
                                v = self.auxvar(tp)
                                ret.append(v)
                                seq.append(
                                    AssignExpr(FieldExpr(mk_deref(aa),
                                                         *fieldpath), v))
                        else:
                            v = self.auxvar(aa.type.dereference)
                            ret.append(v)
                            seq.append(AssignExpr(mk_deref(aa), v))
                    else:
                        args.append(aa)
                ee = CallExpr(func, args, ret)
                if seq:
                    seq.insert(0, ee)
                    return SeqExpr(*seq)
                else:
                    return ee
            else:
                return super(OutParReplacer, self).visit_CallExpr(e)

        def visit_DerefExpr(self, e):
            if isinstance(e.lval, VarExpr) and e.lval.var in self.outpvars:
                return VarExpr(e.type, e.lval.var)
            return DerefExpr(self.visit(e.lval))

        def visit_FieldExpr(self, e):
            if isinstance(e.record, DerefExpr) \
               and isinstance(e.record.lval, VarExpr) \
               and e.record.lval.var in self.outpvars:
                return VarExpr(e.type, '%s->%s' %
                               (e.record.lval.var, '->'.join(e.fieldpath)))
            return FieldExpr(self.visit(e.record), *e.fieldpath)

    # end of class OutParReplacer
    

    for fn in program.functions:
        outpvars = set()
        if fn.name in outmap:
            outpars = outmap[fn.name]
            params = []
            for i, p in enumerate(fn.params):
                if i in outpars:
                    f = outpars[i]
                    if f:
                        for (fieldpath, tp) in sorted(f):
                            fn.retvars.append(
                                VarExpr(tp, '%s->%s' % (p.var,
                                                        '->'.join(fieldpath))))
                            outpvars.add(p.var)
                    else:
                        fn.retvars.append(VarExpr(p.type.dereference, p.var))
                        outpvars.add(p.var)
                else:
                    params.append(p)
            fn.params = params
            
        v = OutParReplacer(outmap, outpvars,
                           program.globals, fn.params, fn.retvars, fn.locals)
        fn.body = v.visit(fn.body)
        fn.locals += v.vp.added

    return program
        

def add_field_addrof_assumptions(program):
    class RecordAddrOfFinder(ExprVisitor):
        def __init__(self):
            self.reclocs = {}

        def visit_AddrOfExpr(self, e):
            l = e.lval
            if isinstance(l, (VarExpr, ArrAtExpr)) \
                   and isinstance(l.type, RecordType):
                self.reclocs.setdefault(l.type, []).append(l)
            elif isinstance(l, FieldExpr):
                self.reclocs.setdefault(l.record.type, []).append(l.record)

        def visit_ArrAtExpr(self, e):
            if isinstance(e.type, RecordType):
                self.reclocs.setdefault(e.type, []).append(e)

        def visit_CallExpr(self, e):
            self.visit(e.func)
            for a in e.args:
                self.visit(a)
            for r in e.ret:
                if isinstance(r.type, PointerType) \
                   and isinstance(r.type.dereference, RecordType):
                    self.reclocs.setdefault(r.type.dereference, []).append(
                        DerefExpr(r))

    # end of class RecordAddrOfFinder

    class RecordAddrOfAssumptions(IdentityVisitor):
        def __init__(self, reclocs):
            self.reclocs = reclocs
            self.fired = False

        def mk_addrof(self, e):
            if isinstance(e, DerefExpr):
                return e.lval
            return AddrOfExpr(e)

        def visit_AddrOfExpr(self, e):
            l = e.lval
            if isinstance(l, FieldExpr) \
                   and isinstance(l.record, (DerefExpr, ArrAtExpr)):
                a = self.mk_addrof(l.record)
                assumps = []
                seen = set()
                for r in self.reclocs.get(l.record.type, []):
                    if l.record.equals(r):
                        continue
                    key = str(r)
                    if key in seen:
                        continue
                    seen.add(key)
                    b = self.mk_addrof(r)
                    fe = self.mk_addrof(FieldExpr(r, *l.fieldpath))
                    pre = OpExpr(OpExpr.EQUAL, a, b)
                    conc = OpExpr(OpExpr.EQUAL, e, fe)
                    assumps.append(AssumeExpr(
                        OpExpr(OpExpr.OR, OpExpr(OpExpr.NOT, pre), conc)))
                if assumps:
                    assumps.append(e)
                    self.fired = True
                    return CompExpr(*assumps)
            return e

    # end of class RecordAddrOfAssumptions

    gscope = set(v.var for v in program.globals)
    for fn in program.functions:
        rf = RecordAddrOfFinder()
        rf.visit(fn.body)
        ra = RecordAddrOfAssumptions(rf.reclocs)
        fn.body = ra.visit(fn.body)
        if ra.fired:
            cr = CompRemover(program.globals, fn.params, fn.retvars, fn.locals)
            fn.body = cr.visit(fn.body)
            fn.locals += cr.vp.added

    return program


def get_size_of(tp):
    ret = 0
    for t in get_record_leaf_types(tp):
        if isinstance(t, ArrayType) and t.size:
            ret += t.size * get_size_of(t.element)
        else:
            ret += 1
    return ret
    #return len(list(get_record_leaf_types(tp)))


def get_addr_offset(ptrtype, tp, fieldpath):
    off = 0
    for field in fieldpath:
        for f in tp.fields:
            field_tp = tp.fields[f]
            if field == f:
                tp = field_tp
                break
            off += get_size_of(field_tp)
    return ConstExpr(ptrtype, str(off))


def translate_ptr_type(mem_address_type, tp, fun_ptr_to_enum=None):
    if fun_ptr_to_enum is not None \
           and isinstance(tp, PointerType) \
           and isinstance(tp.dereference, FunctionType) \
           and tp in fun_ptr_to_enum:
        return fun_ptr_to_enum[tp]
    if isinstance(tp, (PointerType, ArrayType)):
        return mem_address_type
    elif isinstance(tp, RecordType):
        return RecordType(
            tp.name,
            OrderedDict((n, translate_ptr_type(mem_address_type, f,
                                               fun_ptr_to_enum))
                        for n, f in tp.fields.items()))
    elif isinstance(tp, FunctionType):
        return FunctionType([translate_ptr_type(mem_address_type, a,
                                                fun_ptr_to_enum)
                             for a in tp.args],
                            [translate_ptr_type(mem_address_type, r,
                                                fun_ptr_to_enum)
                             for r in tp.ret])
    elif isinstance(tp, MapType):
        return MapType(translate_ptr_type(mem_address_type, tp.index,
                                          fun_ptr_to_enum),
                       translate_ptr_type(mem_address_type, tp.element,
                                          fun_ptr_to_enum))
    return tp


class MemoryInfo(object):
    def __init__(self, mem_address_type, mem, memaddr, fun_ptr_to_enum):
        self.mem_address_type = mem_address_type
        self.mem = mem
        self.memaddr = memaddr
        self.fun_ptr_to_enum = fun_ptr_to_enum

    def t_ptr(self, tp):
        return translate_ptr_type(self.mem_address_type, tp,
                                  self.fun_ptr_to_enum)

# end of class MemoryInfo


def remove_pointers(program, mem_address_type, fun_ptr_info, no_pointers,
                    malloc_function_name=None):
    class MemLocFinder(ExprVisitor):
        def __init__(self, ptrtype, mems, locs, loctypes, toalloc):
            self.ptrtype = ptrtype
            self.mems = mems
            self.locs = locs
            self.loctypes = loctypes
            self.toalloc = toalloc

        def visit_AddrOfExpr(self, e):
            self.mems.add(e.lval.type)
            self.visit(e.lval)
            if isinstance(e.lval, VarExpr):
                if e.lval.var not in self.locs:
                    self.locs[e.lval.var] = VarExpr(
                        self.ptrtype, '&' + e.lval.var)
                    self.loctypes[e.lval.var] = e.lval.type
            elif isinstance(e.lval, FieldExpr):
                r = e.lval.record
                if isinstance(r, VarExpr):
                    if r.var not in self.locs:
                        self.locs[r.var] = VarExpr(
                            self.ptrtype, '&' + r.var)
                        self.loctypes[r.var] = r.type
                        self.mems.add(r.type)
                elif isinstance(r, DerefExpr) and isinstance(r.lval, VarExpr):
                    pass
                elif isinstance(r, ArrAtExpr):
                    pass
                else:
                    e.write(sys.stderr)
                    assert False
            elif isinstance(e.lval, (DerefExpr, ArrAtExpr)):
                pass
            elif isinstance(e.lval, ConstExpr) \
                     and isinstance(e.lval.type, FunctionType):
                pass
            else:
                e.lval.write(sys.stderr)
                assert False

        def visit_DerefExpr(self, e):
            self.visit(e.lval)
            self.mems.add(e.type)
                          
        def visit_VarExpr(self, e):
            if isinstance(e.type, PointerType):
                if not isinstance(e.type.dereference, VoidType):
                    self.mems.add(e.type.dereference)
            elif isinstance(e.type, ArrayType):
                self.mems.add(e.type.element)
                self.locs[e.var] = VarExpr(self.ptrtype, '&' + e.var)
                self.loctypes[e.var] = e.type

        def visit_ArrAtExpr(self, e):
            self.visit(e.array)
            self.visit(e.index)
            if isinstance(e.array, FieldExpr) \
               and isinstance(e.array.record, VarExpr):
                key = (tuple(e.array.fieldpath), e.type)
                self.toalloc.setdefault(e.array.record.var, {})[key] = e.array
            self.mems.add(e.type)

    # end of class MemLocFinder

    fun_ptr_to_enum = {}
    for tp in fun_ptr_info:
        values = []
        if tp.args:
            # start with functions of type "[] -> ret" with a fixed
            # ordering; this guarantees that the typecast from the
            # enum representing "[] -> ret" to the enum representing
            # the union of "[] -> ret" and "args -> ret" preserves the
            # enum values
            arbitrary_param_fun = FunctionType([], tp.ret)
            values += sorted('&%s' % fn.value for fn in fun_ptr_info.get(arbitrary_param_fun, set()))

        values += sorted('&%s' % fn.value for fn in fun_ptr_info[tp])
        etp = EnumType(values)
        etp._enumvals = { v : i for (i, v) in enumerate(values) }
        fun_ptr_to_enum[PointerType(tp)] = etp

    def t_ptr(tp):
        return translate_ptr_type(mem_address_type, tp, fun_ptr_to_enum)

    mapat_name_map = {}
    cnt = [0]
    def mk_mapat_name(m):
        name = str(m)
        ret = mapat_name_map.get(name)
        if ret is None:
            ret = '%%ptrvar[%d]' % cnt[0]
            cnt[0] += 1
            mapat_name_map[name] = ret
        return ret

    class PtrRemover(IdentityVisitor):
        def __init__(self, locs, mem, ptrtype):
            self.locs = locs
            self.mem = mem
            self.ptrtype = ptrtype
            self.added = {}

        def mapat_expr(self, tp, idx):
            ret = MapAtExpr(self.mem[tp], idx)
            if no_pointers:
                name = mk_mapat_name(ret)
                ret = VarExpr(ret.type, name)
                self.added[name] = ret
            return ret

        def visit_VarExpr(self, e):
            if isinstance(e.type, PointerType) \
                   and isinstance(e.type.dereference, FunctionType):
                tp = fun_ptr_to_enum.get(e.type, self.ptrtype)
                return VarExpr(tp, e.var)
            elif isinstance(e.type, ArrayType):
                return self.locs[e.var]
            elif e.var in self.locs:
                assert e.type in self.mem, str(e)
                return self.mapat_expr(e.type, self.locs[e.var])
            else:
                return VarExpr(t_ptr(e.type), e.var)

        def visit_AddrOfExpr(self, e):
            l = e.lval
            if isinstance(l, VarExpr):
                return self.locs[l.var]
            elif isinstance(l, FieldExpr):
                if isinstance(l.record, VarExpr):
                    return OpExpr(OpExpr.ADD, self.locs[l.record.var],
                                  get_addr_offset(self.ptrtype,
                                                  l.record.type, l.fieldpath))
                elif isinstance(l.record, DerefExpr) \
                         and isinstance(l.record.lval, VarExpr):
                    # r = l.record
                    # name = '%s->%s' % (r.lval.var, '->'.join(l.fieldpath))
                    # v = VarExpr(self.ptrtype, name)
                    # self.added[name] = v
                    # return v
                    ptr = self.visit(l.record.lval)
                    return OpExpr(OpExpr.ADD, ptr,
                                  get_addr_offset(self.ptrtype,
                                                  l.record.type, l.fieldpath))
                elif isinstance(l.record, ArrAtExpr):
                    arr = self.visit(AddrOfExpr(l.record))
                    return OpExpr(OpExpr.ADD, arr,
                                  get_addr_offset(self.ptrtype,
                                                  l.record.type, l.fieldpath))
            elif isinstance(l, DerefExpr):
                return self.visit(l.lval)
            elif isinstance(l, ArrAtExpr):
                arr = self.mk_ptr(self.visit(l.array))
                idx = self.mk_ptr(self.visit(l.index))
                idx = OpExpr(OpExpr.MUL,
                             ConstExpr(self.ptrtype, get_size_of(l.type)), idx)
                return OpExpr(OpExpr.ADD, arr, idx)
            elif isinstance(l, ConstExpr) and isinstance(l.type, FunctionType):
                return ConstExpr(fun_ptr_to_enum[e.type], '&%s' % l.value)
            else:
                import sys
                print(str(l.__class__))
                l.write(sys.stderr)
                assert False

        def visit_DerefExpr(self, e):
            idx = self.mk_ptr(self.visit(e.lval))
            return self.mapat_expr(e.type, idx)

        def visit_ArrAtExpr(self, e):
            idx = self.mk_ptr(self.visit(e.array))
            off = OpExpr(OpExpr.MUL,
                         ConstExpr(self.ptrtype, get_size_of(e.type)),
                         self.mk_ptr(self.visit(e.index)))
            addr = OpExpr(OpExpr.ADD, idx, off)

            if isinstance(e.type, ArrayType):
                # multidimensional array; return the offset, not its contents
                return addr

            return self.mapat_expr(e.type, addr)

        def visit_FieldExpr(self, e):
            if isinstance(e.record, VarExpr) \
               and e.record.var in self.locs:
                return FieldExpr(
                    self.mapat_expr(e.record.type, self.locs[e.record.var]),
                    *e.fieldpath)
            else:
                return FieldExpr(self.visit(e.record), *e.fieldpath)

        def visit_TypeCastExpr(self, e):
            tp = t_ptr(e.type)
            a = self.visit(e.expr)
            if a.type != tp:
                a = TypeCastExpr(tp, a, e.is_bitcast)
            return a

        def visit_ConstExpr(self, e):
            ret = ConstExpr(t_ptr(e.type), e.value)
            if isinstance(e.type, PointerType) \
               and isinstance(e.type.dereference, FunctionType) \
               and ret.value == '0' and isinstance(ret.type, EnumType):
                # special case -- NULL function pointers
                ret.value = ret.type.values[-1]
            return ret

        def visit_OpExpr(self, e):
            tp = e.type
            args = [self.visit(a) for a in e.args]
            if isinstance(tp, PointerType) and e.op in (OpExpr.ADD, OpExpr.SUB):
                f = ConstExpr(self.ptrtype, get_size_of(tp.dereference))
                args[1] = OpExpr(OpExpr.MUL, f, args[1])
            return OpExpr(e.op, *args)

        def mk_ptr(self, e):
            if e.type != self.ptrtype:
                e = TypeCastExpr(self.ptrtype, e, 1)
            return e

    # end of class PtrRemover

    gvars = dict((v.var, v) for v in program.globals)
    
    locs = {}
    loctypes = {}
    toalloc = {}
    #vp = VarProvider(program.globals)
    mems = set()
    if program.init:
        mf = MemLocFinder(mem_address_type, mems, locs, loctypes, toalloc)
        mf.visit(program.init)
    flocs = {}
    floctypes = {}
    ftoalloc = {}
    for fn in program.functions:
        flocs[fn.name] = {}
        floctypes[fn.name] = {}
        ftoalloc[fn.name] = {}
        mf = MemLocFinder(mem_address_type, mems,
                          flocs[fn.name], floctypes[fn.name], ftoalloc[fn.name])
        for p in fn.params:
            mf.visit(p)
        for r in fn.retvars:
            mf.visit(r)
        for v in fn.locals:
            mf.visit(v)
        mf.visit(fn.body)

    for fn in program.functions:
        fl = flocs[fn.name]
        ft = floctypes[fn.name]
        fa = ftoalloc[fn.name]
        for n in list(fl):
            if n in gvars:
                locs[n] = fl[n]
                loctypes[n] = ft[n]
                del fl[n]
                del ft[n]
                if n in fa:
                    toalloc[n] = fa[n]
                    del fa[n]
        for n in list(fa):
            if n in gvars:
                toalloc[n] = fa[n]
                del fa[n]

    mem = {}
    memaddr = VarExpr(mem_address_type, '%memaddr')
    for (i, tp) in enumerate(sorted(mems, key=str)):
        if isinstance(tp, FunctionType):
            continue
        tpp = t_ptr(tp)
        m = mem.get(tpp)
        if m is None:
            assert not isinstance(tpp, VoidType), (str(tp), str(tpp))
            m = VarExpr(MapType(mem_address_type, tpp), '%%mem.%d' % i)
            mem[tpp] = m
        mem[tp] = m

    if program.init:
        rem = PtrRemover(locs, mem, mem_address_type)
        program.init = rem.visit(program.init)

    malloc_size = VarExpr(memaddr.type, '%size')
    malloc_ret = VarExpr(memaddr.type, '%ret')
    malloc_body = SeqExpr(
        AssignExpr(malloc_ret, memaddr),
        AssignExpr(memaddr, OpExpr(OpExpr.ADD, memaddr, malloc_size)))
    malloc_func = Function(
        '%allocate',
        [malloc_size],
        [malloc_ret],
        [],
        malloc_body)

    glblallocs = []

    for fn in program.functions:
        curlocs = flocs[fn.name]
        curloctypes = floctypes[fn.name]

        own = dict((v.var, v) for v in
                   itertools.chain(fn.locals, fn.params, fn.retvars))

        cur_and_global_locs = {}
        cur_and_global_locs.update(curlocs)
        cur_and_global_locs.update((k,v) for k,v in locs.items() if k not in own)

        rem = PtrRemover(cur_and_global_locs, mem, mem_address_type)

        def process(vl, assumps=None):
            for v in vl:
                vv = rem.visit(v)
                if isinstance(vv, VarExpr):
                    yield vv
                elif assumps is not None:
                    assumps.append(AssumeExpr(OpExpr(OpExpr.EQUAL, vv, v)))
                    yield v

        def mk_alloc(seq, key):
            seq.append(CallExpr(malloc_func.get_const(),
                                [ConstExpr(memaddr.type,
                                           get_size_of(curloctypes[key]))],
                                [curlocs[key]]))

        assumps = []
        fn.params = list(process(fn.params, assumps))
        fn.retvars = list(process(fn.retvars, assumps))
        fn.locals = list(process(fn.locals))
        locset = set(v.var for v in
                     itertools.chain(fn.locals, fn.params, fn.retvars))

        allocs = []
        for name in curlocs:
            assert name in own
            if curlocs[name].var not in locset:
                fn.locals.append(curlocs[name])
            mk_alloc(allocs, name)
            # else:
            #     locs[name] = curlocs[name]
            #     loctypes[name] = curloctypes[name]

        for name, fields in ftoalloc[fn.name].items():
            assert name not in gvars, (name, sorted(gvars))
            for f in fields.values():
                allocs.append(
                    CallExpr(malloc_func.get_const(),
                             [ConstExpr(memaddr.type,
                                        get_size_of(f.type))], [f]))
            # else:
            #     toalloc.setdefault(name, {}).update(fields)
                
        body = rem.visit(fn.body)
        allocs += assumps
        if allocs:
            fn.body = SeqExpr(*(allocs + [body]))
        else:
            fn.body = body

        for name in sorted(rem.added):
            fn.locals.append(rem.added[name])

    for fn in program.functions:
        if fn.name == program.entrypoint:
            allocs = []
            for name in locs:
                if name in gvars:
                    allocs.append(
                        CallExpr(malloc_func.get_const(),
                                 [ConstExpr(memaddr.type,
                                            get_size_of(loctypes[name]))],
                                 [locs[name]]))
                    
            for name, fields in toalloc.items():
                for f in fields.values():
                    allocs.append(
                        CallExpr(malloc_func.get_const(),
                                 [ConstExpr(memaddr.type,
                                            get_size_of(f.type))], [f]))

            if mem:
                allocs = [AssignExpr(memaddr,
                                     ConstExpr(mem_address_type, '1'))] + allocs
            if allocs:
                fn.body = SeqExpr(*(allocs + [fn.body]))
            break

    if mem:
        program.functions.insert(0, malloc_func)

    glbls = []
    if mem:
        glbls.append(memaddr)
    for v in program.globals:
        if isinstance(v.type, PointerType) \
           and isinstance(v.type.dereference, FunctionType):
            glbls.append(VarExpr(fun_ptr_to_enum[v.type], v.var))
        elif v.var not in locs:
            glbls.append(VarExpr(t_ptr(v.type), v.var))
    memvars = set()
    for tp in sorted(mem, key=str):
        if not isinstance(tp, RecordType):
            v = mem[tp]
            if v.var not in memvars:
                glbls.append(v)
                memvars.add(v.var)
    for name in locs:
        assert name in gvars, name
        glbls.append(locs[name])
    program.globals = glbls

    if malloc_function_name and mem:
        def malloc_call(size, ret):
            return CallExpr(malloc_func.get_const(), [size], [ret])

        try:
            f = program.get_function(malloc_function_name)
            assert len(f.params) == 1 and len(f.retvars) == 1
            f.body = malloc_call(f.params[0], f.retvars[0])
        except KeyError:
            program.functions.insert(
                0,
                Function(
                    malloc_function_name,
                    [malloc_size], [malloc_ret], [],
                    malloc_call(malloc_size, malloc_ret)))

    return program, MemoryInfo(mem_address_type, mem, memaddr, fun_ptr_to_enum)


def remove_string_constants(program, opts):
    stringmap = {}
    char_type = IntType() if not opts.bitvectors else BVType(8, True)
    idx_type = IntType() if not opts.bitvectors else BVType(opts.bitvectors, True)
    
    class StringConstFinder(ExprVisitor):
        def visit_ConstExpr(self, e):
            if isinstance(e.type, SymbolType) and e.type.name == 'string':
                stringmap[e.value] = None

    # end of class StringConstFinder

    sf = StringConstFinder()
    if program.init:
        sf.visit(program.init)

    for f in program.functions:
        sf.visit(f.body)

    vp = VarProvider(program.globals)
    stringkeys = sorted(list(stringmap))
    for i, s in enumerate(stringkeys):
        tp = ArrayType(char_type, len(s)+1)
        v = vp.mkvar(tp, '$string.const.%d' % i)
        stringmap[s] = v
        program.globals.append(v)

    class StringConstReplacer(IdentityVisitor):
        def visit_ConstExpr(self, e):
            if isinstance(e.type, SymbolType) and e.type.name == 'string':
                v = stringmap[e.value]
                return TypeCastExpr(PointerType(v.type.element), v, 1)
            return e

        def visit_AssignExpr(self, e):
            l = self.visit(e.lhs)
            r = e.rhs
            if isinstance(l.type, ArrayType) and \
               isinstance(r.type, SymbolType) and r.type.name == 'string':
                v = stringmap[e.value]
                seq = []
                for i in range(min(l.type.size, v.type.size)):
                    idx = ConstExpr(idx_type, str(i))
                    seq.append(AssignExpr(ArrAtExpr(l, idx), ArrAtExpr(r, idx)))
                if seq:
                    return SeqExpr(*seq)
                else:
                    return AssumeExpr(ConstExpr(True))
            else:
                r = self.visit(r)
                return AssignExpr(l, r)

    # end of class StringConstReplacer

    if stringmap:
        sr = StringConstReplacer()
        if program.init:
            program.init = sr.visit(program.init)

        for f in program.functions:
            f.body = sr.visit(f.body)
            
        entry = program.get_function(program.entrypoint)
        seq = []
        for s in stringkeys:
            v = stringmap[s]
            for i, c in enumerate(s):
                idx = ConstExpr(idx_type, str(i))
                seq.append(AssignExpr(ArrAtExpr(v, idx),
                                      ConstExpr(char_type, str(ord(c)))))
            seq.append(AssignExpr(ArrAtExpr(v, ConstExpr(idx_type, str(len(s)))),
                                  ConstExpr(char_type, '0')))
        seq.append(entry.body)
        entry.body = SeqExpr(*seq)

    return program
            

def remove_function_pointers(program, opts=None):
    funmap = {}
    dflt = {}

    if opts is not None and opts.annotate_labels:
        def _label(name, purpose):
            res = LabelExpr(name)
            res.annotations = [('label-type', purpose)]
            return res
    else:
        def _label(name, purpose):
            return LabelExpr(name)

    class ImplicitFunPtrAdder(IdentityVisitor):
        def __init__(self, pointerized):
            self.pointerized = pointerized

        def visit_ConstExpr(self, e):
            if e.value in self.pointerized:
                return DerefExpr(self.pointerized[e.value])

            return e

    # end of class ImplicitFunPtrAdder

    class ImplicitCallAddrofAdder(IdentityVisitor):
        def __init__(self, pointerized_indices):
            self.pointerized_indices = pointerized_indices

        def visit_CallExpr(self, e):
            if not isinstance(e.func, ConstExpr):
                return e

            cur_pointerized_indices = self.pointerized_indices[e.func.value]
            if not cur_pointerized_indices:
                return e

            new_args = [arg for arg in e.args]
            for i in cur_pointerized_indices:
                new_args[i] = AddrOfExpr(new_args[i])

            return CallExpr(e.func, new_args, e.ret)

    # end of class ImplicitCallAddrofAdder

    pointerized_indices = defaultdict(list)

    for fn in program.functions:
        pointerized = {}
        new_params = []

        for i, param in enumerate(fn.params):
            if isinstance(param, VarExpr) \
               and isinstance(param.type, FunctionType):
                # function parameter has a function type, change it to a
                # function pointer
                fun_ptr = VarExpr(PointerType(param.type), param.var)
                new_params.append(fun_ptr)
                pointerized[param.var] = fun_ptr
                pointerized_indices[fn.name].append(i)
            else:
                new_params.append(param)

        fn.params = new_params

        if pointerized:
            # dereference all the occurences of the parameter in the body
            ifpa = ImplicitFunPtrAdder(pointerized)
            fn.body = ifpa.visit(fn.body)

    # add addrof for arguments passed to the parameters that have been changed to
    # function pointers
    icaa = ImplicitCallAddrofAdder(pointerized_indices)
    for fn in program.functions:
        fn.body = icaa.visit(fn.body)

    class FunPtrFinder(ExprVisitor):
        def visit_AddrOfExpr(self, e):
            l = e.lval
            if isinstance(l, ConstExpr) and isinstance(l.type, FunctionType):
                funmap.setdefault(l.type, set()).add(l)
            return self.generic_visit(e)

        def visit_DerefExpr(self, e):
            if isinstance(e.type, FunctionType):
                dflt[e.type] = None
            return self.generic_visit(e)

    # end of class FunPtrFinder

    class LabelCollector(ExprVisitor):
        def __init__(self):
            self.labels = set()
            
        def visit_LabelExpr(self, e):
            self.labels.add(e.name)

    # end of class LabelCollector

    fpf = FunPtrFinder()
    fvp = VarProvider()
    for fn in program.functions:
        fpf.visit(fn.body)
        fvp.seen.add(fn.name)

    extrafuns = []

    vp = VarProvider(program.globals)
    for fn in program.functions:
        vp.seen.add(fn.name)
    for tp in dflt:
        params = [vp.mkvar(t) for t in tp.args]
        ret = [vp.mkvar(t) for t in tp.ret]
        body = []
        for v in program.globals:
            if isinstance(v.type, ArrayType):
                idx_type = IntType() if opts is None or not opts.bitvectors \
                    else BVType(opts.bitvectors, 1)
                for i in range(v.type.size):
                    idx = ConstExpr(idx_type, str(i))
                    body.append(HavocExpr(ArrAtExpr(v, idx)))
            else:
                body.append(HavocExpr(v))
        for v in ret:
            body.append(HavocExpr(v))
        name = vp.mkvar(tp, 'unknown.function.ptr').var
        if not body:
            body = [AssumeExpr(ConstExpr(True))]
        f = Function(name, params, ret, [], SeqExpr(*body))
        extrafuns.append(f)
        dflt[tp] = f.get_const()


    # collect functions of type "[] -> ret", which can be called with
    # arbitrary arguments
    arbitrary_param_funs = defaultdict(list)
    for tp, funs in funmap.items():
        if not tp.args:
            arbitrary_param_funs[tuple(tp.ret)].extend(funs)

    class FunPtrCallRemover(IdentityVisitor):
        def __init__(self, vp):
            self.vp = vp

        def visit_CallExpr(self, e):
            f = e.func
            if isinstance(f, DerefExpr):
                if isinstance(f.lval, AddrOfExpr):
                    return CallExpr(f.lval.lval, e.args, e.ret)
                else:
                    return self.make_call_table(f, e.args, e.ret)
            return super(FunPtrCallRemover, self).visit_CallExpr(e)

        def make_call_table(self, f, args, ret):
            # functions of type "[] -> ret", can be called with arbitrary arguments
            funcs = sorted(arbitrary_param_funs[tuple(f.type.ret)], key=lambda c : c.value)
            # add actual functions of type "args -> ret"
            funcs += sorted(funmap.get(f.type, set()), key=lambda c : c.value)

            lbls = [_label(self.vp.mkvar(LabelType(),
                                         'call.%s' % c.value).var, 'switch-case')
                    for c in funcs]
            lbls.append(_label(self.vp.mkvar(LabelType(),
                                             'call.default').var, 'switch-case'))
            end = _label(self.vp.mkvar(LabelType(), 'call.end').var,
                         'switch-case')
            seq = []
            seq.append(JumpExpr(*lbls))
            elsecond = ConstExpr(True)
            for i, fn in enumerate(funcs):
                if fn.type != f.type:
                    # if the function was of type "[] -> ret" and not
                    # "args -> ret", typecast the function pointer to
                    # the actual type of fn
                    addr = TypeCastExpr(PointerType(f.type), AddrOfExpr(fn), False)
                else:
                    addr = AddrOfExpr(fn)

                thiscond = OpExpr(OpExpr.EQUAL, f.lval, addr)
                elsecond = OpExpr(OpExpr.AND, elsecond,
                                  OpExpr(OpExpr.NOT, thiscond))
                seq.append(lbls[i])
                seq.append(AssumeExpr(thiscond))
                # do not pass the arguments if the function is of type "[] -> ret"
                seq.append(CallExpr(fn, args if fn.type.args else [], ret))
                seq.append(JumpExpr(end))
            seq.append(lbls[-1])
            seq.append(AssumeExpr(elsecond))
            seq.append(CallExpr(dflt[f.type], args, ret))
            seq.append(end)
            return SeqExpr(*seq)

    # end of class FunPtrCallRemover

    for fn in program.functions:
        lc = LabelCollector()
        lc.visit(fn.body)
        vp = VarProvider()
        vp.seen = lc.labels
        rm = FunPtrCallRemover(vp)
        fn.body = rm.visit(fn.body)

    for fn in extrafuns:
        funmap.setdefault(fn.get_type(), set()).add(fn.get_const())

    program.functions += extrafuns
    return program, funmap


def remove_records(program, meminfo):
    class MapFinder(ExprVisitor):
        def __init__(self):
            self.recordmem = set()
            self.seen = set()

        def visit_type(self, tp):
            if tp not in self.seen:
                self.seen.add(tp)
                if isinstance(tp, MapType):
                    if isinstance(tp.element, RecordType):
                        self.recordmem.add(tp)
                    self.visit_type(tp.index)
                    self.visit_type(tp.element)
                elif isinstance(tp, RecordType):
                    for f in tp.fields:
                        self.visit_type(tp.fields[f])

        def generic_visit(self, e):
            self.visit_type(e.type)
            super(MapFinder, self).generic_visit(e)

    # end of class MapFinder
            
    class FieldRemover(IdentityVisitor):
        def __init__(self):
            self.added = {}
            
        def visit_FieldExpr(self, e):
            if isinstance(e.record, VarExpr):
                name = '%s->%s' % (e.record.var, '->'.join(e.fieldpath))
                v = VarExpr(meminfo.t_ptr(e.type), name)
                self.added.setdefault(e.record.var, []).append(v)
                return v
            elif isinstance(e.record, MapAtExpr):
                r = self.visit(e.record.index)
                mem = meminfo.mem[e.type]
                itp = mem.type.index
                def _cast(r):
                    if r.type != itp:
                        return TypeCastExpr(itp, r, False)
                    return r
                idx = OpExpr(OpExpr.ADD, _cast(r),
                             get_addr_offset(mem.type.index,
                                             e.record.type, e.fieldpath))
                ret = MapAtExpr(mem, idx)
                return ret
            elif isinstance(e.record, ConstExpr):
                val = e.record.value
                for i in e.fieldpath:
                    val = val[i]
                return copy.copy(val)
            else:
                print('UNEXPECTED:', str(e))
                assert False

        def visit_AssignExpr(self, e):
            if isinstance(e.lhs.type, RecordType) \
               and isinstance(e.rhs, ConstExpr):
                return AssumeExpr(ConstExpr(True))
            return super(FieldRemover, self).visit_AssignExpr(e)

    # end of class FieldRemover

    mf = MapFinder()
    glbls = set()
    for v in program.globals:
        mf.visit(v)
        glbls.add(v.var)
    if program.init:
        mf.visit(program.init)
    for fn in program.functions:
        mf.visit(fn.body)

    for i, m in enumerate(mf.recordmem):
        for j, tp in enumerate(get_record_leaf_types(m.element)):
            tpp = meminfo.t_ptr(tp)
            mv = meminfo.mem.get(tpp)
            if mv is None:
                mv = VarExpr(MapType(meminfo.mem_address_type, tp),
                             '%%mem.r.%d.%d' % (i, j))
                meminfo.mem[tpp] = mv
                program.globals.append(mv)
            meminfo.mem[tp] = mv

    def is_record_type(tp):
        if isinstance(tp, RecordType):
            return True
        elif isinstance(tp, MapType):
            return is_record_type(tp.index) or is_record_type(tp.element)
        return False

    program.globals = [v for v in program.globals if not is_record_type(v.type)]

    for fn in program.functions:
        fr = FieldRemover()
        fn.body = fr.visit(fn.body)
        fn.locals = [v for v in fn.locals if not is_record_type(v.type)]
        for rname in sorted(fr.added):
            vl = fr.added[rname]
            if rname in glbls:
                for v in vl:
                    if v.var not in glbls:
                        program.globals.append(v)
                        glbls.add(v.var)
            else:
                vn = set()
                for v in vl:
                    if v.var not in vn:
                        vn.add(v.var)
                        fn.locals.append(v)
            ## assert isinstance(fr.added[name], VarExpr)
            ## fn.locals.append(fr.added[name])

    return program


def flatten_havocs(program):
    class HavocFlattener(IdentityVisitor):
        def __init__(self, *scopes):
            self.vp = VarProvider(*scopes)

        def visit_HavocExpr(self, e):
            l = self.visit(e.lval)
            if not isinstance(l, VarExpr):
                v = self.vp.auxvar(l.type)
                return SeqExpr(HavocExpr(v), AssignExpr(l, v))
            else:
                return HavocExpr(l)

    # end of class HavocFlattener

    return transform_program(program, HavocFlattener)


def promote_enums(program, bitvectors):
    to_promote = set()

    class EnumPromoteCollector(ExprVisitor):
        def visit_TypeCastExpr(self, e):
            self.visit(e.expr)
            if isinstance(e.expr.type, EnumType):
                to_promote.add(e.expr.type)
            if isinstance(e.type, EnumType):
                to_promote.add(e.type)

        def visit_OpExpr(self, e):
            bad_op = e.op != OpExpr.EQUAL
            for a in e.args:
                self.visit(a)
                if bad_op and isinstance(a.type, EnumType):
                    to_promote.add(a.type)

    # end of class EnumPromoteCollector

    epc = EnumPromoteCollector()
    
    if program.init:
        epc.visit(program.init)

    for fn in program.functions:
        epc.visit(fn.body)

    if not to_promote:
        return program

    enum2bv = {}
    if bitvectors:
        for tp in to_promote:
            signed = False
            bits = 1
            if hasattr(tp, '_enumvals'):
                vals = sorted(tp._enumvals[v] for v in tp.values)
                if not vals:
                    enum2bv[tp] = BVType(1, False)
                    continue
                if vals[0] < 0:
                    signed = True
                m = max(abs(vals[0]), abs(vals[-1]))
                lbits = math.log(m+1, 2)
                bits = int(math.ceil(lbits))
                if signed:
                    bits += 1
            else:
                bits = int(math.ceil(math.log(len(tp.values), 2)))
            bits = max(bits, 1)
            enum2bv[tp] = BVType(bits, signed)

    def translate_enum_type(tp):
        if tp in to_promote:
            if bitvectors:
                return enum2bv[tp]
            else:
                return IntType()
        elif isinstance(tp, FunctionType):
            return FunctionType([translate_enum_type(a) for a in tp.args],
                                [translate_enum_type(r) for r in tp.ret])
        elif isinstance(tp, MapType):
            return MapType(translate_enum_type(tp.index),
                           translate_enum_type(tp.element))
        return tp

    class EnumPromoteReplacer(IdentityVisitor):
        def visit_VarExpr(self, e):
            return VarExpr(translate_enum_type(e.type), e.var)

        def visit_ConstExpr(self, e):
            tp = translate_enum_type(e.type)
            val = e.value
            if tp != e.type and isinstance(e.type, EnumType):
                val = str(self.get_enum_int_val(e.type, e.value))
                ## try:
                ##     val = str(e.type._enumvals[e.value])
                ## except KeyError:
                ##     v = int(e.value)
                ##     found = False
                ##     for k in e.type._enumvals:
                ##         if e.type._enumvals[k] == v:
                ##             val = v
                ##             found = True
                ##             break
                ##     if not found:
                ##         raise
            return ConstExpr(tp, val)

        def visit_TypeCastExpr(self, e):
            return TypeCastExpr(translate_enum_type(e.type),
                                self.visit(e.expr), e.is_bitcast)

        def visit_HavocExpr(self, e):
            lval = self.visit(e.lval)
            ret = HavocExpr(lval)
            if lval.type != e.lval.type and isinstance(e.lval.type, EnumType):
                ivals = []
                for val in e.lval.type.values:
                    ival = self.get_enum_int_val(e.lval.type, val)
                    ivals.append(ival)
                ivals.sort()
                constr = None
                if not ivals:
                    constr = ConstExpr(True)
                else:
                    lo, hi = ivals[0], ivals[-1]
                    if ivals == list(range(lo, hi+1)):
                        constr = OpExpr(OpExpr.AND,
                                        OpExpr(OpExpr.GE, lval,
                                               ConstExpr(lval.type, str(lo))),
                                        OpExpr(OpExpr.LE, lval,
                                               ConstExpr(lval.type, str(hi))))
                    else:
                        for ival in ivals:
                            eq = OpExpr(OpExpr.EQUAL, lval,
                                        ConstExpr(lval.type, str(ival)))
                            if constr is None:
                                constr = eq
                            else:
                                constr = OpExpr(OpExpr.OR, constr, eq)
                ret = SeqExpr(ret, AssumeExpr(constr))
            return ret

        def get_enum_int_val(self, tp, strval):
            try:
                return tp._enumvals[strval]
            except KeyError:
                v = int(strval)
                for k in tp._enumvals:
                    if tp._enumvals[k] == v:
                        return v
                raise
            except AttributeError:
                assert False, (str(tp), strval)

    # end of class EnumPromoteReplacer

    repl = EnumPromoteReplacer()
    
    program.globals = [repl.visit(v) for v in program.globals]
    if program.init:
        program.init = repl.visit(program.init)

    for fn in program.functions:
        fn.body = repl.visit(fn.body)
        fn.params = [repl.visit(p) for p in fn.params]
        fn.retvars = [repl.visit(r) for r in fn.retvars]
        fn.locals = [repl.visit(l) for l in fn.locals]

    return program


def remove_mapat(program):
    class MapAtRemover(IdentityVisitor):
        def visit_AssignExpr(self, e):
            l = e.lhs
            r = self.visit(e.rhs)
            
            if isinstance(l, MapAtExpr):
                # a[0][1][2] = 3
                # (mapat (mapat (mapat a 0) 1) 2) = 3
                # a0 = (mapget a 0)
                # a01 = (mapget a0 1)
                # a012 = (mapset a01 2 3)
                # a01 = (mapset a0 1 a012)
                # a0 = (mapset a 0 a01)
                # a = a0
                idx = []
                while isinstance(l, MapAtExpr):
                    idx.append(self.visit(l.index))
                    l = l.map
                assert isinstance(l, VarExpr), str(l)
                m = l
                val = r
                idx.reverse()
                sidx = []
                for i in idx[:-1]:
                    sidx.append((m, i))
                    m = OpExpr(OpExpr.MAPGET, m, i)
                aa = OpExpr(OpExpr.MAPSET, m, idx[-1], val)
                sidx.reverse()
                for (mm, i) in sidx:
                    aa = OpExpr(OpExpr.MAPSET, mm, i, aa)
                res = AssignExpr(l, aa)
            else:
                res = AssignExpr(l, r)
            return res

        def visit_MapAtExpr(self, e):
            return OpExpr(OpExpr.MAPGET, self.visit(e.map), self.visit(e.index))

    # end of class MapAtRemover

    mr = MapAtRemover()
    if program.init:
        program.init = mr.visit(program.init)

    for fn in program.functions:
        fn.body = mr.visit(fn.body)

    return program


def remove_unused_globals(program):
    glbls = set(v.var for v in program.globals)
    seen = set()
    
    class SeenVisitor(ExprVisitor):
        def visit_VarExpr(self, e):
            if e.var in glbls:
                seen.add(e.var)

    # end of class SeenVisitor

    sv = SeenVisitor()
    if program.init:
        sv.visit(program.init)

    for f in program.functions:
        sv.visit(f.body)

    g = []
    for v in program.globals:
        if v.var in seen:
            g.append(v)
    program.globals = g

    return program


class LabelsCounter(ExprVisitor):
    def __init__(self):
        self.count = {}
        self.seen = set()

    def visit_JumpExpr(self, e):
        for t in e.targets:
            self.count[t.name] = self.count.get(t.name, 0) + 1

    def visit_CondJumpExpr(self, e):
        self.count[e.target.name] = self.count.get(e.target.name, 0) + 1

    def visit_LabelExpr(self, e):
        self.seen.add(e.name)

# end of class LabelsCounter
    

class StaticLoopInfo(object):
    def __init__(self, bound, idx, lc, incrseq):
        self.bound = bound
        self.idx = idx
        self.lc = lc
        self.incrseq = incrseq

    def pre_loop(self):
        return None

    def pre_body(self, i):
        return None

    def post_body(self, i):
        return None

    def post_loop(self):
        return None

# end of class StaticLoopInfo


def is_simple_assign(init, i):
    if isinstance(init, CompExpr) and len(init.args) == 2 \
       and isinstance(init.args[0], AssignExpr) \
       and init.args[0].lhs.equals(init.args[1]):
        init = init.args[0]
    elif isinstance(init, SeqExpr) and len(init.args) == 1:
        init = init.args[0]

    if isinstance(init, AssignExpr) \
       and init.lhs.equals(i) \
       and isinstance(init.rhs, ConstExpr) \
       and isinstance(init.rhs.type, (IntType, BVType)):
        return int(init.rhs.value)
    return None


def is_simple_incr(incr, i):
    if isinstance(incr, CompExpr) and len(incr.args) == 2 \
       and isinstance(incr.args[0], AssignExpr) \
       and incr.args[0].lhs.equals(i) \
       and incr.args[1].equals(i) \
       and isinstance(incr.args[0].rhs, OpExpr) \
       and incr.args[0].rhs.op == OpExpr.ADD \
       and incr.args[0].rhs.args[0].equals(i) \
       and isinstance(incr.args[0].rhs.args[1], ConstExpr) \
       and isinstance(incr.args[0].rhs.args[1].type, (IntType, BVType)):
        return int(incr.args[0].rhs.args[1].value)
    return None


def is_never_written(body, i):
    class Written(Exception):
        pass

    class Visitor(ExprVisitor):
        def __init__(self):
            self.scope = []

        def visit_AssignExpr(self, e):
            self.scope.append(True)
            self.visit(e.lhs)
            self.scope.pop()
            self.visit(e.rhs)

        def visit_CallExpr(self, e):
            self.scope.append(True)
            self.visit(e.func)
            for a in e.ret:
                self.visit(a)
            self.scope.pop()
            for a in e.args:
                self.visit(a)

        def visit_ArrAtExpr(self, e):
            self.visit(e.array)
            self.scope.append(False)
            self.visit(e.index)
            self.scope.pop()

        def visit_AddrOfExpr(self, e):
            self.scope.append(True)
            self.visit(e.lval)
            self.scope.pop()

        def generic_visit(self, e):
            self.scope.append(False)
            for c in e.children():
                self.visit(c)
            self.scope.pop()

        def visit_FieldExpr(self, e):
            self.scope.append(True)
            for c in e.children():
                self.visit(c)
            self.scope.pop()

        def visit_VarExpr(self, e):
            if e.var == i.var and self.scope and self.scope[-1]:
                raise Written()

    # end of class Visitor

    v = Visitor()
    try:
        v.visit(body)
        return True
    except Written:
        return False


def get_loop_incr_sequence(n, i, e):
    initval = is_simple_assign(e.init, i)
    incrval = is_simple_incr(e.incr, i)
    if initval is None or incrval is None or \
           not is_never_written(e.body, i):
        initval = None
        incrseq = None
    else:
        incrseq = []
        for j in range(n):
            curval = initval + j*incrval
            incrseq.append(ConstExpr(i.type, str(curval)))
        curval = initval + n * incrval
        incrseq.append(ConstExpr(i.type, str(curval)))
    return incrseq


def is_static_loop(function, e, labels_count):
    guard = e.guard
    if isinstance(guard, CompExpr) and len(guard.args) == 2 \
       and isinstance(guard.args[0], AssignExpr) \
       and isinstance(guard.args[1], VarExpr) \
       and isinstance(guard.args[0].lhs, VarExpr) \
       and guard.args[1].var == guard.args[0].lhs.var:
        guard = guard.args[0].rhs
    if isinstance(guard, OpExpr) \
       and guard.op in (OpExpr.LT, OpExpr.LE) \
       and len(guard.args) == 2 \
       and isinstance(guard.args[0], VarExpr) \
       and isinstance(guard.args[1], ConstExpr) \
       and isinstance(guard.args[1].type, (IntType, BVType)):
        lc = LabelsCounter()
        lc.visit(e.body)
        for name in lc.seen:
            if labels_count.get(name, 0) > lc.count.get(name, 0):
                # there are jumps from outside the loop!
                return False, None
        n = int(guard.args[1].value)
        if guard.op == OpExpr.LE:
            n += 1
        i = guard.args[0]

        return True, StaticLoopInfo(n, i, lc, get_loop_incr_sequence(n, i, e))
    return False, None  
    

def unroll_static_loops(program, static_loop_checker=is_static_loop, **kwds):
    opts = kwds.get('opts')
    annotate_labels = opts and opts.annotate_labels
    
    class ForLoopUnroller(IdentityVisitor):
        def __init__(self, func, count, vp):
            self.func = func
            self.count = count
            self.vp = vp
            
        def visit_ForExpr(self, e):
            init = self.visit(e.init)
            guard = self.visit(e.guard)
            incr = self.visit(e.incr)
            body = self.visit(e.body)
            e = ForExpr(init, guard, incr, body)
            res, info = self._is_static_loop(e)
            if not res:
                return e
            i = info.idx
            n = info.bound
            lc = info.lc
            # replace the loop with n copies of the body
            incrseq = info.incrseq
            seq = []
            def add_if(expr):
                if expr is not None:
                    seq.append(expr)
            if incrseq is not None:
                # this is a simple loop
                add_if(info.pre_loop())
                for j in range(n):
                    cur = incrseq[j]
                    seq.append(AssignExpr(i, cur))
                    b = self._rename_labels(body, j, lc)
                    add_if(info.pre_body(j))
                    seq.append(self._substitute(i, cur, b))
                    add_if(info.post_body(j))
                seq.append(AssignExpr(i, incrseq[n]))
                add_if(info.post_loop())
            else:
                add_if(info.pre_loop())
                seq.append(init)
                for j in range(n):
                    add_if(info.pre_body(j))
                    seq.append(self._rename_labels(body, j, lc))
                    seq.append(self._rename_labels(e.incr, j, lc))
                    add_if(info.post_body(j))
                add_if(info.post_loop())
            return SeqExpr(*seq)

        def _is_static_loop(self, e):
            return static_loop_checker(self.func, e, self.count)

        def _rename_labels(self, e, i, lc):
            class LabelRenamer(IdentityVisitor):
                def __init__(self):
                    self.lblname = {}
                    for name in lc.seen:
                        n = vp.mkvar(LabelType(), '%s#%d' % (name, i)).var
                        self.lblname[name] = n
                    
                def visit_LabelExpr(self, e):
                    ret = LabelExpr(self.lblname.get(e.name, e.name))
                    ret.annotations = e.annotations
                    if ret.annotations and annotate_labels:
                        ret.annotations = [
                            (k, v) for (k, v) in ret.annotations
                            if k != 'label-type' or v != 'loop-for'
                        ]
                    return ret

            # end of class LabelRenamer
                
            r = LabelRenamer()
            return r.visit(e)

        def _substitute(self, i, val, e):
            class SubstVisitor(IdentityVisitor):
                def visit_VarExpr(self, e):
                    if e.var == i.var:
                        return copy.copy(val)
                    return VarExpr(e.type, e.var)

            # end of class SubstVisitor

            s = SubstVisitor()
            return s.visit(e)

    # end of class ForLoopUnroller

    if program.init:
        un = ForLoopUnroller(None, {}, VarProvider())
        program.init = un.visit(program.init)

    for fn in program.functions:
        vp = VarProvider(program.globals, fn.locals, fn.params, fn.retvars)
        lc = LabelsCounter()
        lc.visit(fn.body)
        un = ForLoopUnroller(fn, lc.count, vp)
        fn.body = un.visit(fn.body)

    return program


def abstract_external_functions(program, malloc_function_name=None):
    allfuncs = set(f.name for f in program.functions)
    if malloc_function_name:
        allfuncs.add(malloc_function_name)

    class FunAbstractor(IdentityVisitor):
        def visit_CallExpr(self, e):
            if isinstance(e.func, ConstExpr) and e.func.value not in allfuncs:
                if not e.ret:
                    return AssumeExpr(ConstExpr(True))
                else:
                    return SeqExpr(*[HavocExpr(r) for r in e.ret])
            else:
                return e
            
    # end of class FunAbstractor

    for f in program.functions:
        f.body = FunAbstractor().visit(f.body)
    return program


def apply_rw(opts, func, program, *args):
    name = getattr(func, 'name', None) or func.__name__
    if opts.verbose:
        start = time.time()
        sys.stderr.write(';; %s ...' % name)
        sys.stderr.flush()
    ret = func(program, *args)
    gc.collect()
    if opts.verbose:
        end = time.time()
        sys.stderr.write(' %.3f sec\n' % (end - start))
        sys.stderr.flush()
    if opts.debug:
        sys.stderr.write(';; after application of %s\n' % name)
        if isinstance(ret, tuple):
            ret[0].write(sys.stderr)
        else:
            ret.write(sys.stderr)
        sys.stderr.flush()
    return ret


class Rewrite(object):
    def __init__(self, name, func, *args):
        self.name = name
        self.func = func
        self.args = args

    def __call__(self, program):
        return self.func(program, *self.args)

# end of class Rewrite


def get_rewrites(opts):
    pre, post = [], []

    if opts.no_unreachable:
        pre.append(Rewrite('remove_unreachable',
                           remove_unreachable, opts.entry_point))

    if opts.unroll_static_loops:
        pre.append(Rewrite('unroll_static_loops', unroll_static_loops))

    pre.append(Rewrite('remove_side_effects', remove_side_effects, opts))
    if opts.blast_static_arrays:
        pre.append(Rewrite('blast_static_arrays',
                           blast_static_arrays,
                           opts.blast_static_arrays,
                           opts.blast_static_arrays_allow_var_idx,
                           opts))
    #pre.append(Rewrite('remove_static_arrays', remove_static_arrays))
        
    if opts.detect_output_params:
        post.append(Rewrite('rewrite_output_params', rewrite_output_params))

    return pre, post


def rewrite(opts, program):
    gc_on = gc.isenabled()
    if gc_on:
        gc.disable()
    start = time.time()

    if opts.debug:
        sys.stderr.write(';; after parsing\n')
        program.write(sys.stderr)
        sys.stderr.flush()

    for rw in opts.rewrites[0]:
        program = apply_rw(opts, rw, program)

    if not opts.no_default_rewrites:
        program = apply_rw(opts, remove_string_constants, program, opts)
        program, fun_ptr_info = \
                 apply_rw(opts, remove_function_pointers, program, opts)
        program = apply_rw(opts, remove_static_arrays, program)
        program = apply_rw(opts, remove_side_effects, program)
        if opts.abstract_ext_funs:
            program = apply_rw(opts,
                               abstract_external_functions,
                               program,
                               opts.builtin_malloc)
        program = apply_rw(opts, flatten_fields, program)
        program = apply_rw(opts, expand_records, program, opts)

    for rw in opts.rewrites[1]:
        program = apply_rw(opts, rw, program)

    if not opts.no_default_rewrites:
        program = apply_rw(opts, add_field_addrof_assumptions, program)

        memtype = IntType()
        if opts.bitvectors:
            memtype = BVType(opts.bitvectors, False)
        program, meminfo = apply_rw(opts, remove_pointers, program,
                                    memtype, fun_ptr_info, opts.no_pointers,
                                    opts.builtin_malloc)
        program = apply_rw(opts, remove_records, program, meminfo)
        program = apply_rw(opts, flatten_havocs, program)
        program = apply_rw(opts, promote_enums, program, opts.bitvectors)
        program = apply_rw(opts, remove_mapat, program)
        program = apply_rw(opts, remove_unused_globals, program)

    if len(opts.rewrites) == 3:
        for rw in opts.rewrites[2]:
            program = apply_rw(opts, rw, program)

    end = time.time()
    if opts.verbose:
        sys.stderr.write(';; total time: %.3f sec\n' % (end - start))
        sys.stderr.flush()

    if gc_on:
        gc.enable()
        
    return program

