// -*- C++ -*-
//
// Author: Alberto Griggio <griggio@fbk.eu>
//
// This file is part of Kratos 2.

#include <Python.h>
#include "kratos.h"
#include <vector>
#include <unordered_map>
#include <iostream>
#include <memory>

namespace {

kratos_visit_status expr_c_to_py_converter(kratos_env e, kratos_expr x,
                                           bool preorder, void *user_data);

class PyExprConverter {
public:
    PyExprConverter(kratos_env env, PyObject *module):
        env_(env)
    {
        typemap_.resize(size_t(KRATOS_TYPE_ENUM)+1);
        typemap_[KRATOS_TYPE_VOID] =
            PyObject_GetAttrString(module, "VoidType");
        typemap_[KRATOS_TYPE_LABEL] =
            PyObject_GetAttrString(module, "LabelType");
        typemap_[KRATOS_TYPE_BOOL] =
            PyObject_GetAttrString(module, "BoolType");
        typemap_[KRATOS_TYPE_INT] =
            PyObject_GetAttrString(module, "IntType");
        typemap_[KRATOS_TYPE_REAL] =
            PyObject_GetAttrString(module, "RealType");
        typemap_[KRATOS_TYPE_SYMBOL] =
            PyObject_GetAttrString(module, "SymbolType");
        typemap_[KRATOS_TYPE_BV] =
            PyObject_GetAttrString(module, "BVType");
        typemap_[KRATOS_TYPE_FP] = PyObject_GetAttrString(module, "FPType");
        typemap_[KRATOS_TYPE_MAP] = PyObject_GetAttrString(module, "MapType");
        typemap_[KRATOS_TYPE_FUNCTION] =
            PyObject_GetAttrString(module, "FunctionType");
        typemap_[KRATOS_TYPE_ENUM] =
            PyObject_GetAttrString(module, "EnumType");

        classmap_.resize(size_t(KRATOS_EXPR_CONDJUMP)+1);
        classmap_[KRATOS_EXPR_VAR] = PyObject_GetAttrString(module, "VarExpr");
        classmap_[KRATOS_EXPR_CONST] =
            PyObject_GetAttrString(module, "ConstExpr");
        classmap_[KRATOS_EXPR_OP] = PyObject_GetAttrString(module, "OpExpr");
        classmap_[KRATOS_EXPR_CALL] =
            PyObject_GetAttrString(module, "CallExpr");
        classmap_[KRATOS_EXPR_ASSIGN] =
            PyObject_GetAttrString(module, "AssignExpr");
        classmap_[KRATOS_EXPR_ASSUME] =
            PyObject_GetAttrString(module, "AssumeExpr");
        classmap_[KRATOS_EXPR_TYPECAST] =
            PyObject_GetAttrString(module, "TypeCastExpr");
        classmap_[KRATOS_EXPR_SEQ] = PyObject_GetAttrString(module, "SeqExpr");
        classmap_[KRATOS_EXPR_JUMP] =
            PyObject_GetAttrString(module, "JumpExpr");
        classmap_[KRATOS_EXPR_LABEL] =
            PyObject_GetAttrString(module, "LabelExpr");
        classmap_[KRATOS_EXPR_HAVOC] =
            PyObject_GetAttrString(module, "HavocExpr");
        classmap_[KRATOS_EXPR_CONDJUMP] =
            PyObject_GetAttrString(module, "CondJumpExpr");
    }

    ~PyExprConverter()
    {
        for (auto &p : classmap_) {
            Py_XDECREF(p);
        }
        for (auto &p : typemap_) {
            Py_XDECREF(p);
        }
        for (size_t i = to_unref_.size(); i > 0; --i) {
            Py_XDECREF(to_unref_[i-1]);
        }
    }

    PyObject *operator()(kratos_expr e)
    {
        if (!kratos_visit_expr(env_, e, expr_c_to_py_converter, this)) {
            PyErr_SetString(PyExc_ValueError,
                            kratos_last_error_message(env_));
            return NULL;
        }
        auto res = get(e);
        cache_.clear();
        return res;
    }

    kratos_visit_status visit(kratos_expr e, bool preorder)
    {
        switch (kratos_expr_get_tag(env_, e)) {
        case KRATOS_EXPR_VAR: return visit_var(e, preorder);
        case KRATOS_EXPR_CONST: return visit_const(e, preorder);
        case KRATOS_EXPR_OP: return visit_op(e, preorder);
        case KRATOS_EXPR_CALL: return visit_call(e, preorder);
        case KRATOS_EXPR_ASSIGN: return visit_assign(e, preorder);
        case KRATOS_EXPR_ASSUME: return visit_assume(e, preorder);
        case KRATOS_EXPR_TYPECAST: return visit_typecast(e, preorder);
        case KRATOS_EXPR_SEQ: return visit_seq(e, preorder);
        case KRATOS_EXPR_JUMP: return visit_jump(e, preorder);
        case KRATOS_EXPR_LABEL: return visit_label(e, preorder);
        case KRATOS_EXPR_HAVOC: return visit_havoc(e, preorder);
        case KRATOS_EXPR_CONDJUMP: return visit_condjump(e, preorder);
        default: return KRATOS_VISIT_ABORT;
        }
        return KRATOS_VISIT_ABORT;
    }

    PyObject *sym(kratos_sexp s)
    {
        auto it = symcache_.find(s);
        if (it != symcache_.end()) {
            auto ret = it->second;
            Py_INCREF(ret);
            return ret;
        }
        
        auto v = kratos_symbol_get_value(env_, s);
        auto res = ref(PyUnicode_FromString(v));
        kratos_free(v);
        symcache_[s] = res;
        Py_INCREF(res);
        return res;
    }

    PyObject *constmap(kratos_sexp s)
    {
        if (kratos_sexp_is_nil(env_, s)) {
            Py_INCREF(Py_None);
            return Py_None;
        } else if (kratos_sexp_is_symbol(env_, s)) {
            return sym(s);
        } else {
            PyObject *ret = PyDict_New();
            for (auto cur = s; !kratos_sexp_is_nil(env_, cur);
                 cur = kratos_sexp_cdr(env_, cur)) {
                auto cell = kratos_sexp_car(env_, cur);
                auto k = kratos_sexp_car(env_, cell);
                auto v = kratos_sexp_cdr(env_, cell);
                auto pk = constmap(k);
                auto pv = constmap(v);
                PyDict_SetItem(ret, pk, pv);
                Py_XDECREF(pv);
            }
            return ret;
        }
    }

    PyObject *call(PyObject *o, PyObject *args)
    {
        auto res = ref(PyObject_CallObject(o, args));
        if (args) {
            Py_XDECREF(args);
        }
        return res;
    }

    PyObject *ann(kratos_sexp a)
    {
        PyObject *l = PyList_New(0);
        while (!kratos_sexp_is_nil(env_, a)) {
            auto p = kratos_sexp_car(env_, a);
            a = kratos_sexp_cdr(env_, a);
            auto key = sym(kratos_sexp_car(env_, p));
            PyObject *val = sexp(kratos_sexp_cdr(env_, p));
            if (!val) {
                val = Py_None;
                Py_INCREF(val);
            }
            auto t = Py_BuildValue("(OO)", key, val);
            PyList_Append(l, t);
            //Py_XDECREF(t);
        }
        return l;
    }

private:
    PyObject *get(kratos_expr e)
    {
        auto it = cache_.find(e);
        if (it == cache_.end()) {
            return NULL;
        }
        auto res = it->second;
        Py_INCREF(res);
        return res;
    }

    kratos_visit_status visit_var(kratos_expr e, bool preorder)
    {
        if (!preorder) {
            put(e, call(classmap_[KRATOS_EXPR_VAR],
                        Py_BuildValue("(OO)",
                                      type(e),
                                      sym(kratos_var_expr_get_var(env_, e)))));
        }
        return KRATOS_VISIT_PROCESS;
    }
    
    kratos_visit_status visit_const(kratos_expr e, bool preorder)
    {
        if (!preorder) {
            auto s = kratos_const_expr_get_value(env_, e);
            PyObject *val = nullptr;
            if (kratos_sexp_is_symbol(env_, s)) {
                val = sym(s);
            } else {
                val = constmap(s);
            }
            put(e, call(classmap_[KRATOS_EXPR_CONST],
                        Py_BuildValue("(OO)", type(e), val)));
        }
        return KRATOS_VISIT_PROCESS;
    }
    
    kratos_visit_status visit_op(kratos_expr e, bool preorder)
    {
        if (!preorder) {
            auto args = PyList_New(0);
            auto eargs = kratos_op_expr_get_args(env_, e);
            for (size_t i = 0, n = kratos_op_expr_get_num_args(env_, e);
                 i < n; ++i) {
                auto a = eargs[i];
                PyList_Append(args, cache_[a]);
            }
            kratos_free(eargs);
            auto op = int(kratos_op_expr_get_tag(env_, e))-1;
            put(e, call(classmap_[KRATOS_EXPR_OP],
                        Py_BuildValue("(iN)", op, args)));
        }
        return KRATOS_VISIT_PROCESS;
    }
    
    kratos_visit_status visit_call(kratos_expr e, bool preorder)
    {
        if (!preorder) {
            auto args = PyList_New(0);
            auto ret = PyList_New(0);

            auto eargs = kratos_call_expr_get_args(env_, e);
            for (size_t i = 0, n = kratos_call_expr_get_num_args(env_, e);
                 i < n; ++i) {
                auto a = eargs[i];
                PyList_Append(args, cache_[a]);
            }
            kratos_free(eargs);
            
            auto eret = kratos_call_expr_get_returns(env_, e);
            for (size_t i = 0, n = kratos_call_expr_get_num_returns(env_, e);
                 i < n; ++i) {
                auto r = eret[i];
                PyList_Append(ret, cache_[r]);
            }
            kratos_free(eret);
            
            auto f = cache_[kratos_call_expr_get_function(env_, e)];
            put(e, call(classmap_[KRATOS_EXPR_CALL],
                        Py_BuildValue("(ONN)", f, args, ret)));
        }
        return KRATOS_VISIT_PROCESS;
    }
    
    kratos_visit_status visit_assign(kratos_expr e, bool preorder)
    {
        if (!preorder) {
            auto l = cache_[kratos_assign_expr_get_var(env_, e)];
            auto r = cache_[kratos_assign_expr_get_value(env_, e)];
            put(e, call(classmap_[KRATOS_EXPR_ASSIGN],
                        Py_BuildValue("(OO)", l, r)));
        }
        return KRATOS_VISIT_PROCESS;
    }
    
    kratos_visit_status visit_assume(kratos_expr e, bool preorder)
    {
        if (!preorder) {
            auto c = cache_[kratos_assume_expr_get_cond(env_, e)];
            put(e, call(classmap_[KRATOS_EXPR_ASSUME],
                        Py_BuildValue("(O)", c)));
        }
        return KRATOS_VISIT_PROCESS;
    }
    
    kratos_visit_status visit_typecast(kratos_expr e, bool preorder)
    {
        if (!preorder) {
            auto v = cache_[kratos_typecast_expr_get_expr(env_, e)];
            int b = kratos_typecast_expr_is_bitcast(env_, e);
            put(e, call(classmap_[KRATOS_EXPR_TYPECAST],
                        Py_BuildValue("(OOi)", type(e), v, b)));
        }
        return KRATOS_VISIT_PROCESS;
    }
    
    kratos_visit_status visit_seq(kratos_expr e, bool preorder)
    {
        if (!preorder) {
            auto args = PyList_New(0);
            auto eargs = kratos_seq_expr_get_args(env_, e);
            for (size_t i = 0, n = kratos_seq_expr_get_num_args(env_, e);
                 i < n; ++i) {
                auto a = eargs[i];
                PyList_Append(args, cache_[a]);
            }
            kratos_free(eargs);
            put(e, call(classmap_[KRATOS_EXPR_SEQ],
                        Py_BuildValue("(O)", args)));
        }
        return KRATOS_VISIT_PROCESS;
    }
    
    kratos_visit_status visit_jump(kratos_expr e, bool preorder)
    {
        if (!preorder) {
            auto args = PyList_New(0);
            auto eargs = kratos_jump_expr_get_args(env_, e);
            for (size_t i = 0, n = kratos_jump_expr_get_num_args(env_, e);
                 i < n; ++i) {
                auto a = eargs[i];
                PyList_Append(args, cache_[a]);
            }
            kratos_free(eargs);
            put(e, call(classmap_[KRATOS_EXPR_JUMP],
                        Py_BuildValue("(O)", args)));
        }
        return KRATOS_VISIT_PROCESS;        
    }
    
    kratos_visit_status visit_label(kratos_expr e, bool preorder)
    {
        if (!preorder) {
            put(e, call(classmap_[KRATOS_EXPR_LABEL],
                        Py_BuildValue(
                            "(O)",
                            sym(kratos_label_expr_get_name(env_, e)))));
        }
        return KRATOS_VISIT_PROCESS;
    }
    
    kratos_visit_status visit_havoc(kratos_expr e, bool preorder)
    {
        if (!preorder) {
            auto v = cache_[kratos_havoc_expr_get_var(env_, e)];
            put(e, call(classmap_[KRATOS_EXPR_HAVOC],
                        Py_BuildValue("(O)", v)));
        }
        return KRATOS_VISIT_PROCESS;
    }
    
    kratos_visit_status visit_condjump(kratos_expr e, bool preorder)
    {
        if (!preorder) {
            auto c = cache_[kratos_condjump_expr_get_cond(env_, e)];
            auto t = cache_[kratos_condjump_expr_get_target(env_, e)];
            put(e, call(classmap_[KRATOS_EXPR_CONDJUMP],
                        Py_BuildValue("(OO)", c, t)));
        }
        return KRATOS_VISIT_PROCESS;        
    }

    bool is_list(kratos_sexp s)
    {
        while (true) {
            if (kratos_sexp_is_nil(env_, s)) {
                return true;
            } else if (kratos_sexp_is_symbol(env_, s)) {
                return false;
            }
            s = kratos_sexp_cdr(env_, s);
        }
        return false;
    }
    
    PyObject *sexp(kratos_sexp s)
    {
        if (kratos_sexp_is_nil(env_, s)) {
            return NULL;
        } else if (kratos_sexp_is_symbol(env_, s)) {
            return sym(s);
        } else if (is_list(s)) {
            auto ret = PyList_New(0);
            while (!kratos_sexp_is_nil(env_, s)) {
                auto v = kratos_sexp_car(env_, s);
                s = kratos_sexp_cdr(env_, s);
                auto e = sexp(v);
                if (e != NULL) {
                    PyList_Append(ret, e);
                    //Py_XDECREF(e);
                }
            }
            return ref(ret);
        } else {
            auto a = sexp(kratos_sexp_car(env_, s));
            auto b = sexp(kratos_sexp_cdr(env_, s));
            if (a == NULL && b == NULL) {
                return NULL;
            } else if (a == NULL) {
                return b;
            } else if (b == NULL) {
                return a;
            } else {
                return ref(Py_BuildValue("(OO)", a, b));
            }
        }
    }
    
    void put(kratos_expr e, PyObject *o)
    {
        assert(o);
        auto ea = kratos_expr_get_annotation(env_, e);
        if (!KRATOS_ERROR_OBJ(ea)) {
            auto a = ann(ea);
            PyObject_SetAttrString(o, "annotations", a);
            kratos_sexp_unref(env_, ea);
        }
        cache_[e] = o;
    }
    
    PyObject *ref(PyObject *o)
    {
        to_unref_.push_back(o);
        return o;
    }

    PyObject *type(kratos_expr e)
    {
        return type(kratos_expr_get_type(env_, e));
    }
    
    PyObject *type(kratos_type tp)
    {
        auto it = typecache_.find(tp);
        if (it != typecache_.end()) {
            auto ret = it->second;
            Py_INCREF(ret);
            return ret;
        }
        
        const auto store =
            [&](PyObject *o) -> PyObject *
            {
                typecache_[tp] = o;
                return o;
            };

        auto tag = kratos_type_get_tag(env_, tp);
        auto m = typemap_[tag];
        switch (tag) {
        case KRATOS_TYPE_VOID:
        case KRATOS_TYPE_LABEL:
        case KRATOS_TYPE_BOOL:
        case KRATOS_TYPE_INT:
        case KRATOS_TYPE_REAL:
            return store(call(m, NULL));
        case KRATOS_TYPE_SYMBOL:
            return store(
                call(m, Py_BuildValue(
                         "(O)",
                         sym(kratos_symbol_type_get_name(env_, tp)))));
        case KRATOS_TYPE_BV:
            return store(call(m, Py_BuildValue(
                                  "(ii)",
                                  kratos_bv_type_get_bits(env_, tp),
                                  kratos_bv_type_is_signed(env_, tp))));
        case KRATOS_TYPE_FP:
            return store(call(m, Py_BuildValue(
                                  "(ii)",
                                  kratos_fp_type_get_exponent(env_, tp),
                                  kratos_fp_type_get_mantissa(env_, tp))));
        case KRATOS_TYPE_MAP: {
            auto i = type(kratos_map_type_get_index(env_, tp));
            auto e = type(kratos_map_type_get_element(env_, tp));
            return store(call(m, Py_BuildValue("(OO)", i, e)));
        }
        case KRATOS_TYPE_FUNCTION: {
            size_t a = kratos_function_type_get_num_args(env_, tp);
            size_t r = kratos_function_type_get_num_returns(env_, tp);
            auto args = PyList_New(a);
            auto ret = PyList_New(r);
            auto targs = kratos_function_type_get_args(env_, tp);
            for (size_t i = 0; i < a; ++i) {
                auto p = type(targs[i]);
                Py_INCREF(p);
                PyList_SET_ITEM(args, i, p);
            }
            kratos_free(targs);
            auto tret = kratos_function_type_get_returns(env_, tp);
            for (size_t i = 0; i < r; ++i) {
                auto p = type(tret[i]);
                Py_INCREF(p);
                PyList_SET_ITEM(ret, i, p);
            }
            kratos_free(tret);
            return store(call(m, Py_BuildValue("(NN)", args, ret)));
        }
        case KRATOS_TYPE_ENUM: {
            size_t n = kratos_enum_type_get_num_values(env_, tp);
            auto tvals = kratos_enum_type_get_values(env_, tp);
            auto vals = PyList_New(n);
            for (size_t i = 0; i < n; ++i) {
                auto s = sym(tvals[i]);
                Py_INCREF(s);
                PyList_SET_ITEM(vals, i, s);
            }
            kratos_free(tvals);
            return store(call(m, Py_BuildValue("(N)", vals)));
        }
        default:
            return NULL;
        }
        return NULL;
    }

    kratos_env env_;

    std::vector<PyObject *> classmap_;
    std::vector<PyObject *> typemap_;
    template <class Repr>
    struct Repr_hash_eq {
        size_t operator()(const Repr &r) const
        {
            return reinterpret_cast<size_t>(r.repr);
        }

        bool operator()(const Repr &a, const Repr &b) const
        {
            return a.repr == b.repr;
        }
    };
    std::unordered_map<kratos_expr, PyObject *,
                       Repr_hash_eq<kratos_expr>,
                       Repr_hash_eq<kratos_expr>> cache_;
    std::unordered_map<kratos_type, PyObject *,
                       Repr_hash_eq<kratos_type>,
                       Repr_hash_eq<kratos_type>> typecache_;
    std::unordered_map<kratos_sexp, PyObject *,
                       Repr_hash_eq<kratos_sexp>,
                       Repr_hash_eq<kratos_sexp>> symcache_;
    std::vector<PyObject *> to_unref_;
};


kratos_visit_status expr_c_to_py_converter(kratos_env e, kratos_expr x,
                                           bool preorder, void *user_data)
{
    PyExprConverter *c = static_cast<PyExprConverter *>(user_data);
    return c->visit(x, preorder);
}


bool get_config(PyObject *pyconf, kratos_env env, kratos_config config)
{
    PyObject *key, *value;
    Py_ssize_t pos = 0;

    while (PyDict_Next(pyconf, &pos, &key, &value)) {
        if (!PyUnicode_Check(key)) {
            return false;
        }
        auto k = PyUnicode_AsUTF8(key);
        if (strcmp(k, "symexec_constraint_provider") != 0 &&
            strcmp(k, "symexec_object_monitor") != 0) {
            if (!PyUnicode_Check(value)) {
                return false;
            }
            auto v = PyUnicode_AsUTF8(value);
            if (!kratos_config_set_option(env, config, k, v)) {
                return false;
            }
        }
    }
    return true;
}


PyObject *c_to_py_trace_rec(PyExprConverter &conv,
                            PyObject *path, PyObject *step,
                            kratos_env env, kratos_trace trace, bool first)
{
    if (kratos_trace_is_path(env, trace)) {
        bool err = false;
        PyObject *args = Py_BuildValue("()");
        PyObject *res = PyObject_CallObject(path, args);
        Py_XDECREF(args);

        auto list = kratos_trace_get_path(env, trace);
        for (size_t i = 0, n = kratos_trace_get_path_length(env, trace);
             i < n; ++i) {
            auto o = c_to_py_trace_rec(conv, path, step, env, list[i], false);
            if (o) {
                PyList_Append(res, o);
                //Py_XDECREF(o);
            } else {
                err = true;
                break;
            }
        }
        kratos_free(list);

        if (!err) {
            if (!first) {
                args = Py_BuildValue("(N)", res);
                res = PyObject_CallObject(step, args);
                Py_XDECREF(args);
            }

            return res;
        } else {
            Py_XDECREF(res);
            return NULL;
        }
    } else {
        auto expr = kratos_trace_get_step(env, trace);
        auto res = conv(expr);
        if (res) {
            auto args = Py_BuildValue("(N)", res);
            res = PyObject_CallObject(step, args);
            Py_XDECREF(args);
        }

        return res;
    }
}


PyObject *c_to_py_trace(PyObject *module, kratos_env env, kratos_trace trace)
{
    if (KRATOS_ERROR_OBJ(trace)) {
        Py_INCREF(Py_None);
        return Py_None;
    }

    auto path = PyObject_GetAttrString(module, "ExecutionPath");
    auto step = PyObject_GetAttrString(module, "ExecutionStep");
    PyExprConverter conv(env, module);

    PyObject *res = c_to_py_trace_rec(conv, path, step, env, trace, true);

    kratos_trace_unref(env, trace);
    return res;
}


struct PyCallbackData {
    PyExprConverter *conv;
    PyObject *cp;
    PyObject *om;
};


kratos_expr py_symexec_constraint_provider(kratos_env e, kratos_expr v,
                                           kratos_sexp tag, void *user_data)
{
    kratos_expr res = { NULL };
    PyCallbackData *d = static_cast<PyCallbackData *>(user_data);
    auto p = (*d->conv)(v);
    if (p) {
        PyObject *r = NULL;
        auto t = kratos_symbol_get_value(e, tag);
        if (t) {
            auto args = Py_BuildValue("(Ns)", p, t);
            r = PyObject_CallObject(d->cp, args);
            Py_XDECREF(args);
            kratos_free(t);
        }
        Py_XDECREF(p);
        if (r) {
            auto s = PyObject_Str(r);
            Py_XDECREF(r);
            if (s) {
                res = kratos_parse_expr(e, PyUnicode_AsUTF8(s));
                Py_XDECREF(s);
            }
        }
    }
    return res;
}


bool py_symexec_object_monitor(kratos_env e, kratos_sexp id,
                               size_t key_size, kratos_expr *key,
                               size_t val_size, kratos_expr *val,
                               void *user_data)
{
    bool res = false;
    PyCallbackData *d = static_cast<PyCallbackData *>(user_data);
    auto i = kratos_symbol_get_value(e, id);
    if (i) {
        auto &conv = *(d->conv);
        PyObject *k = PyList_New(key_size);
        PyObject *v = NULL;
        bool err = false;
        for (size_t j = 0; j < key_size && !err; ++j) {
            auto p = conv(key[j]);
            if (p) {
                PyList_SET_ITEM(k, j, p);
            } else {
                err = true;
            }
        }
        if (val && !err) {
            v = PyList_New(val_size);
            for (size_t j = 0; j < val_size && !err; ++j) {
                PyObject *p = NULL;
                if (KRATOS_ERROR_OBJ(val[j])) {
                    p = Py_None;
                    Py_INCREF(p);
                } else {
                    p = conv(val[j]);
                }
                if (p) {
                    PyList_SET_ITEM(v, j, p);
                } else {
                    err = true;
                }
            }
        } else if (!err) {
            v = Py_None;
            Py_INCREF(v);
        }
        if (!err) {
            auto args = Py_BuildValue("(sNN)", i, k, v);
            auto b = PyObject_CallObject(d->om, args);
            Py_XDECREF(args);
            res = PyObject_IsTrue(b);
            Py_XDECREF(b);
        }
        Py_XDECREF(v);
        Py_XDECREF(k);
    }
    return res;
}


} // namespace


extern "C" {

static PyObject *parse_expr(PyObject *self, PyObject *args)
{
    PyObject *module;
    char *data;
    
    if (!PyArg_ParseTuple(args, "Os", &module, &data)) {
        return NULL;
    }

    PyObject *res = NULL;
    auto env = kratos_create_env();
    auto parsed = kratos_parse_expr(env, data);

    if (KRATOS_ERROR_OBJ(parsed)) {
        PyErr_SetString(PyExc_ValueError, kratos_last_error_message(env));
        kratos_destroy_env(env);
        return NULL;
    }
        
    PyExprConverter conv(env, module);
    res = conv(parsed);
    kratos_expr_unref(env, parsed);
    kratos_destroy_env(env);
    
    return res;
}


static PyObject *parse_program(PyObject *self, PyObject *args)
{
    PyObject *module;
    char *data;
    
    if (!PyArg_ParseTuple(args, "Os", &module, &data)) {
        return NULL;
    }

    auto env = kratos_create_env();
    auto prog = kratos_parse_program(env, data);

    if (KRATOS_ERROR_OBJ(prog)) {
        PyErr_SetString(PyExc_ValueError, kratos_last_error_message(env));
        kratos_destroy_env(env);
        return NULL;
    }

    PyExprConverter conv(env, module);

    auto mkprog = PyObject_GetAttrString(module, "Program");
    auto mkfunc = PyObject_GetAttrString(module, "Function");

    auto glbls = PyList_New(0);
    auto funcs = PyList_New(0);

    kratos_expr *pglbls = NULL;
    kratos_function *pfuncs = NULL;

    PyObject *init = NULL;
    kratos_expr pinit;
    PyObject *entry = NULL;
    kratos_sexp pentry;

    PyObject *py_prog = NULL;
    
    pglbls = kratos_program_get_globals(env, prog);
    for (size_t i = 0, n = kratos_program_get_num_globals(env, prog);
         i < n; ++i) {
        auto v = pglbls[i];
        auto p = conv(v);
        if (!p) {
            kratos_free(pglbls);
            goto cleanup;
        }
        PyList_Append(glbls, p);
        //Py_XDECREF(p);
    }
    kratos_free(pglbls);
        
    pfuncs = kratos_program_get_functions(env, prog);
    for (size_t i = 0, n = kratos_program_get_num_functions(env, prog);
         i < n; ++i) {
        auto f = pfuncs[i];
        auto nparams = kratos_function_get_num_params(env, f);
        auto nretvars = kratos_function_get_num_returns(env, f);
        auto nlocals = kratos_function_get_num_locals(env, f);
        auto params = PyList_New(nparams);
        auto retvars = PyList_New(nretvars);
        auto locals = PyList_New(nlocals);

        auto fparams = kratos_function_get_params(env, f);
        for (size_t j = 0; j < nparams; ++j) {
            auto v = fparams[j];
            auto p = conv(v);
            if (!p) {
                kratos_free(fparams);
                kratos_free(pfuncs);
                goto cleanup;
            }
            PyList_SET_ITEM(params, j, p);
        }
        kratos_free(fparams);

        auto fretvars = kratos_function_get_returns(env, f);
        for (size_t j = 0; j < nretvars; ++j) {
            auto v = fretvars[j];
            auto p = conv(v);
            if (!p) {
                kratos_free(fretvars);
                kratos_free(pfuncs);
                goto cleanup;
            }
            PyList_SET_ITEM(retvars, j, p);
        }
        kratos_free(fretvars);

        auto flocals = kratos_function_get_locals(env, f);
        for (size_t j = 0; j < nlocals; ++j) {
            auto v = flocals[j];
            auto p = conv(v);
            if (!p) {
                kratos_free(flocals);
                kratos_free(pfuncs);
                goto cleanup;
            }
            PyList_SET_ITEM(locals, j, p);
        }
        kratos_free(flocals);
        
        auto fbody = kratos_function_get_body(env, f);
        auto body = conv(fbody);
        if (!body) {
            kratos_free(pfuncs);
            goto cleanup;
        }

        auto name = kratos_function_get_name(env, f);
        auto py_f = conv.call(mkfunc,
                              Py_BuildValue("(ONNNN)",
                                            conv.sym(name),
                                            params,
                                            retvars,
                                            locals,
                                            body));
        auto ea = kratos_function_get_annotation(env, f);
        if (!KRATOS_ERROR_OBJ(ea)) {
            auto a = conv.ann(ea);
            PyObject_SetAttrString(py_f, "annotations", a);
            kratos_sexp_unref(env, ea);
        }
        PyList_Append(funcs, py_f);
        //Py_XDECREF(py_f);
    }
    kratos_free(pfuncs);

    init = Py_None;
    pinit = kratos_program_get_init(env, prog);
    if (!KRATOS_ERROR_OBJ(pinit)) {
        init = conv(pinit);
        if (!init) {
            goto cleanup;
        }
    } else {
        Py_INCREF(init);
    }

    entry = Py_None;
    pentry = kratos_program_get_entry_point(env, prog);
    if (!KRATOS_ERROR_OBJ(pentry)) {
        entry = conv.sym(pentry);
    } else {
        Py_INCREF(entry);
    }

    py_prog = conv.call(mkprog,
                        Py_BuildValue("(NNNN)",
                                      glbls, init, entry, funcs));

    Py_INCREF(py_prog);
    kratos_program_unref(env, prog);
    kratos_destroy_env(env);
    
    return py_prog;

  cleanup:
    PyErr_SetString(PyExc_ValueError, kratos_last_error_message(env));
    kratos_program_unref(env, prog);
    kratos_destroy_env(env);
    return NULL;
}


static PyObject *get_version(PyObject *self, PyObject *args)
{
    if (!PyArg_ParseTuple(args, "")) {
        return NULL;
    }

    char *v = kratos_get_version();
    PyObject *res = PyUnicode_FromString(v);
    kratos_free(v);
    return res;
}


static PyObject *verify(PyObject *self, PyObject *args)
{
    int method;
    PyObject *conf;
    PyObject *module;
    PyObject *program;
    
    if (!PyArg_ParseTuple(args, "OiO!O", &module, &method,
                          &PyDict_Type, &conf, &program)) {
        return NULL;
    }

    auto sprog = PyObject_Str(program);
    if (!sprog) {
        return NULL;
    }
    
    kratos_trace trace = { NULL };
    auto env = kratos_create_env();
    auto config = kratos_new_config(env);
    auto prog = kratos_parse_program(env, PyUnicode_AsUTF8(sprog));
    Py_XDECREF(sprog);

    PyObject *res = NULL;
    kratos_result status = KRATOS_RESULT_ERROR;

    std::unique_ptr<PyCallbackData> cb;
    std::unique_ptr<PyExprConverter> conv;

    if (KRATOS_ERROR_OBJ(prog)) {
        PyErr_SetString(PyExc_ValueError, kratos_last_error_message(env));
        kratos_destroy_env(env);
        return NULL;
    }

    if (!get_config(conf, env, config)) {
        PyErr_SetString(PyExc_ValueError, "invalid config");
        goto cleanup;
    }

    if (method == KRATOS_VERIFICATION_SYMBOLIC_EXECUTION) {
        conv.reset(new PyExprConverter(env, module));
        cb.reset(new PyCallbackData());
        cb->conv = conv.get();
        cb->cp = PyDict_GetItemString(conf, "symexec_constraint_provider");
        cb->om = PyDict_GetItemString(conf, "symexec_object_monitor");
        if (cb->cp) {
            kratos_config_set_symexec_constraint_provider(
                env, config, py_symexec_constraint_provider, cb.get());
        }
        if (cb->om) {
            kratos_config_set_symexec_object_monitor(
                env, config, py_symexec_object_monitor, cb.get());
        }
    }

    status = kratos_verify(env, kratos_verification_method(method), config,
                           prog, &trace);
    res = Py_BuildValue("(iO)", status, c_to_py_trace(module, env, trace));

  cleanup:
    kratos_config_unref(env, config);
    kratos_program_unref(env, prog);
    kratos_destroy_env(env);

    return res;
}


static PyObject *encode(PyObject *self, PyObject *args)
{
    PyObject *conf;
    PyObject *program;
    
    if (!PyArg_ParseTuple(args, "O!O", &PyDict_Type, &conf, &program)) {
        return NULL;
    }

    auto env = kratos_create_env();
    auto config = kratos_new_config(env);
    auto sprog = PyObject_Str(program);
    auto prog = kratos_parse_program(env, PyUnicode_AsUTF8(sprog));
    Py_XDECREF(sprog);

    PyObject *res = NULL;
    char *res_data = NULL;

    if (KRATOS_ERROR_OBJ(prog)) {
        PyErr_SetString(PyExc_ValueError, kratos_last_error_message(env));
        kratos_destroy_env(env);
        return NULL;
    }

    if (!get_config(conf, env, config)) {
        PyErr_SetString(PyExc_ValueError, "invalid config");
        goto cleanup;
    }

    res_data = kratos_encode(env, config, prog);
    if (res_data) {
        res = PyUnicode_FromString(res_data);
        kratos_free(res_data);
    } else {
        PyErr_SetString(PyExc_ValueError, kratos_last_error_message(env));
    }

  cleanup:
    kratos_config_unref(env, config);
    kratos_program_unref(env, prog);
    kratos_destroy_env(env);
    
    return res;
}


static PyMethodDef _pykratos_methods[] = {
    {"parse_expr", (PyCFunction)parse_expr, METH_VARARGS,
     "Parse an expression"},
    {"parse_program", (PyCFunction)parse_program, METH_VARARGS,
     "Parse a program"},
    {"get_version", (PyCFunction)get_version, METH_VARARGS,
     "Get the Kratos version info"},
    {"encode", (PyCFunction)encode, METH_VARARGS,
     "Encode a K2 program into a transition system"},
    {"verify", (PyCFunction)verify, METH_VARARGS,
     "Verify a K2 program"},
    {NULL, NULL, 0, NULL}
};


static struct PyModuleDef _pykratosmodule = {
    PyModuleDef_HEAD_INIT,
    "_pykratos",
    NULL,
    -1,
    _pykratos_methods
};


PyMODINIT_FUNC
PyInit__pykratos(void)
{
    return PyModule_Create(&_pykratosmodule);
}

} // extern "C"
