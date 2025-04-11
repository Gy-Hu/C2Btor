/* -*- C -*-
 *
 * Author: Alberto Griggio <griggio@fbk.eu>
 *
 * This file is part of Kratos 2.
 */

#ifndef KRATOS_H_INCLUDED
#define KRATOS_H_INCLUDED

#include <stdbool.h>
#include <stdlib.h>

/**
 * \file kratos.h
 *
 * \brief API for Kratos2.
 */

#ifdef __cplusplus
extern "C" {
#endif

/**
 * \name Data structures and special values
 */
/*@{*/

/**
 * \brief Kratos environment.
 */
typedef struct { void *repr; } kratos_env;

/**
 * \brief Kratos S-expression (used to represent symbols and generic
 * structured data).
 */
typedef struct { void *repr; } kratos_sexp;

/**
 * \brief Kratos type.
 */
typedef struct { void *repr; } kratos_type;

/**
 * \brief Kratos expression.
 */
typedef struct { void *repr; } kratos_expr;

/**
 * \brief Kratos function.
 */
typedef struct { void *repr; } kratos_function;

/**
 * \brief Kratos program.
 */
typedef struct { void *repr; } kratos_program;

/**
 * \brief Kratos counterexample trace.
 */
typedef struct { void *repr; } kratos_trace;

/**
 * \brief Kratos configuration.
 */
typedef struct { void *repr; } kratos_config;

/**
 * \brief Error checking macro.
 */
#define KRATOS_ERROR_OBJ(o) ((o).repr == NULL)

/**
 * \brief Kratos type tags.
 */
typedef enum {
    KRATOS_TYPE_ERROR,    /**< error */
    KRATOS_TYPE_VOID,     /**< void type */
    KRATOS_TYPE_LABEL,    /**< label type */
    KRATOS_TYPE_BOOL,     /**< Boolean type */
    KRATOS_TYPE_INT,      /**< mathematical integer type */
    KRATOS_TYPE_REAL,     /**< real (actually rational) type */
    KRATOS_TYPE_SYMBOL,   /**< generic symbolic type */
    KRATOS_TYPE_BV,       /**< fixed-size bit-vector type */
    KRATOS_TYPE_FP,       /**< IEEE floating-point type */
    KRATOS_TYPE_MAP,      /**< mapping type */
    KRATOS_TYPE_FUNCTION, /**< function type */
    KRATOS_TYPE_ENUM      /**< symbolic enum type */
} kratos_type_tag;

/**
 * \brief Kratos expression tags.
 */
typedef enum {
    KRATOS_EXPR_ERROR,    /**< error */
    KRATOS_EXPR_VAR,      /**< variable */
    KRATOS_EXPR_CONST,    /**< constant */
    KRATOS_EXPR_OP,       /**< operation */
    KRATOS_EXPR_CALL,     /**< function call */
    KRATOS_EXPR_ASSIGN,   /**< assignment */
    KRATOS_EXPR_ASSUME,   /**< assumption */
    KRATOS_EXPR_TYPECAST, /**< type conversion */
    KRATOS_EXPR_SEQ,      /**< sequence of statements */
    KRATOS_EXPR_JUMP,     /**< jump */
    KRATOS_EXPR_LABEL,    /**< label */
    KRATOS_EXPR_HAVOC,    /**< havoc */
    KRATOS_EXPR_CONDJUMP  /**< conditional jump */
} kratos_expr_tag;

/**
 * \brief Kratos operation tags.
 */
typedef enum {
    KRATOS_OP_ERROR,      /**< error */
    KRATOS_OP_ADD,        /**< addition */
    KRATOS_OP_SUB,        /**< subtraction */
    KRATOS_OP_MUL,        /**< multiplication */
    KRATOS_OP_NEG,        /**< negation */
    KRATOS_OP_DIV,        /**< division */
    KRATOS_OP_REM,        /**< remainder */
    KRATOS_OP_LSHIFT,     /**< left shift */
    KRATOS_OP_RSHIFT,     /**< right shift */
    KRATOS_OP_BITAND,     /**< bitwise and */
    KRATOS_OP_BITOR,      /**< bitwise or */
    KRATOS_OP_BITXOR,     /**< bitwise xor */
    KRATOS_OP_BITNOT,     /**< bitwise complement */
    KRATOS_OP_EQUAL,      /**< equality */
    KRATOS_OP_AND,        /**< logical and */
    KRATOS_OP_OR,         /**< logical or */
    KRATOS_OP_NOT,        /**< logical not */
    KRATOS_OP_LE,         /**< less or equal */
    KRATOS_OP_LT,         /**< less than */
    KRATOS_OP_GE,         /**< greater or equal */
    KRATOS_OP_GT,         /**< greater than */
    KRATOS_OP_FLOOR,      /**< floor */
    KRATOS_OP_ISFINITE,   /**< test whether a float is finite */
    KRATOS_OP_ISINF,      /**< test whether a float is infinite */
    KRATOS_OP_ISNAN,      /**< test whether a float is NaN */
    KRATOS_OP_ISNORMAL,   /**< test whether a float is normal */
    KRATOS_OP_ISSUBNORMAL,/**< test whether a float is subnormal */
    KRATOS_OP_ISZERO,     /**< test whether a float is zero */
    KRATOS_OP_MAPGET,     /**< map read */
    KRATOS_OP_MAPSET,     /**< map write */
    KRATOS_OP_ITE         /**< if-then-else */
} kratos_op_tag;

/**
 * \brief Kratos result.
 */
typedef enum {
    KRATOS_RESULT_ERROR,   /**< error */
    KRATOS_RESULT_UNKNOWN, /**< unknown */
    KRATOS_RESULT_SAFE,    /**< safe */
    KRATOS_RESULT_UNSAFE   /**< unsafe */
} kratos_result;

/**
 * \brief Kratos verification method.
 */
typedef enum {
    KRATOS_VERIFICATION_MODEL_CHECKING,     /**< (symbolic) model checking */
    KRATOS_VERIFICATION_SYMBOLIC_EXECUTION, /**< symbolic execution */
    KRATOS_VERIFICATION_SIMULATION          /**< (random) simulation */
} kratos_verification_method;

/**
 * \brief Kratos status for the callback passed to ::kratos_visit_expr()
 */
typedef enum {
    KRATOS_VISIT_PROCESS, /**< continue visiting */
    KRATOS_VISIT_ABORT,   /**< abort the visit */
    KRATOS_VISIT_SKIP     /**< skip this expression and its children */
} kratos_visit_status;

/**
 * \brief Callback function to visit an expression with ::kratos_visit_expr()
 *
 * This callback function gets called by the visitor for each visited
 * subexpression. The \a preorder flag tells whether the function is called
 * before or after visiting the children. The return value of this function
 * determines how the visit should continue (see ::kratos_visit_status).
 * Note that the \a x expression should not be unreferenced.
 * \a user_data is a opaque pointer to arbitrary data.
 */
typedef kratos_visit_status (*kratos_visit_callback)(kratos_env e,
                                                     kratos_expr x,
                                                     bool preorder,
                                                     void *user_data);

/**
 * \brief Custom constraint provider for symbolic execution
 */
typedef kratos_expr (*kratos_symexec_constraint_provider)(kratos_env e,
                                                          kratos_expr v,
                                                          kratos_sexp tag,
                                                          void *user_data);

/**
 * \brief Custom object monitor for symbolic execution
 */
typedef bool (*kratos_symexec_object_monitor)(kratos_env e, kratos_sexp id,
                                              size_t key_size, kratos_expr *key,
                                              size_t val_size, kratos_expr *val,
                                              void *user_data);

/**
 * \brief Function for deallocating the memory accessible by pointers returned
 *        by Kratos
 */
void kratos_free(void *mem);

/**
 * \brief Gets the current Kratos version.
 * \return A version string, which must be deallocated by the user
 *         with ::kratos_free().
 */ 
char *kratos_get_version(void);

/**
 * \brief Retrieves the last error message generated while operating in the
 *        given enviroment.
 *
 * \param e The environment in which the error occurred.
 * \return A pointer to the last error message generated.
 */
const char *kratos_last_error_message(kratos_env e);

/**
 * \brief Creates a new Kratos environment.
 * \return A new environment. Use ::KRATOS_ERROR_OBJ() to check for errors.
 */
kratos_env kratos_create_env(void);

/**
 * \brief Destroys the given environment.
 */
void kratos_destroy_env(kratos_env e);

/*@}*/ /* end of data structures and special values group */

/**
 * \name Operations on S-expressions (sexps).
 */
/*@{*/

/**
 * \brief Creates a nil sexp.
 */
kratos_sexp kratos_nil(kratos_env e);

/**
 * \brief Creates a cons sexp.
 */
kratos_sexp kratos_cons(kratos_env e, kratos_sexp a, kratos_sexp b);

/**
 * \brief Creates a symbol sexp.
 */
kratos_sexp kratos_symbol(kratos_env e, const char *value);

/**
 * \brief Test for nil sexps.
 */
bool kratos_sexp_is_nil(kratos_env e, kratos_sexp s);

/**
 * \brief Test for symbol sexps.
 */
bool kratos_sexp_is_symbol(kratos_env e, kratos_sexp s);

/**
 * \brief Retrieve the car of a cons sexp.
 * \return A borrowed reference (i.e. the result should not be unreferenced).
 */
kratos_sexp kratos_sexp_car(kratos_env e, kratos_sexp s);

/**
 * \brief Retrieve the cdr of a cons sexp.
 * \return A borrowed reference.
 */
kratos_sexp kratos_sexp_cdr(kratos_env e, kratos_sexp s);

/**
 * \brief Increase the refcount of the given sexp.
 * \return false in case of errors.
 */
bool kratos_sexp_ref(kratos_env e, kratos_sexp s);

/**
 * \brief Decrease the refcount of the given sexp.
 * \return false in case of errors.
 */
bool kratos_sexp_unref(kratos_env e, kratos_sexp s);

/**
 * \brief Get the value of a symbol expression.
 * \return a newly allocated string, which must be freed with ::kratos_free(),
 *         or NULL on error.
 */
char *kratos_symbol_get_value(kratos_env e, kratos_sexp s);

/**
 * \brief Get the id of the given symbol sexp.
 * \return 0 on error.
 */
size_t kratos_symbol_get_id(kratos_env e, kratos_sexp s);

/*@}*/ /* end of sexp group */

/**
 * \name Types creation and manipulation.
 */
/*@{*/

/**
 * \brief Increase the refcount of the given type.
 * \return false on error.
 */
bool kratos_type_ref(kratos_env e, kratos_type t);

/**
 * \brief Decrease the refcount of the given type.
 * \return false on error.
 */
bool kratos_type_unref(kratos_env e, kratos_type t);

/**
 * \brief Get the tag of the given type.
 */
kratos_type_tag kratos_type_get_tag(kratos_env e, kratos_type t);

/**
 * \brief Get the name of the given symbol type.
 * \return A borrowed reference.
 */
kratos_sexp kratos_symbol_type_get_name(kratos_env e, kratos_type t);

/**
 * \brief Get the bit-width of the given bit-vector type.
 * \return 0 on error.
 */
size_t kratos_bv_type_get_bits(kratos_env e, kratos_type t);

/**
 * \brief Get the signedness of the given bit-vector type.
 */
bool kratos_bv_type_is_signed(kratos_env e, kratos_type t);

/**
 * \brief Get the bit-width of the given float type.
 * \return 0 on error.
 */
size_t kratos_fp_type_get_bits(kratos_env e, kratos_type t);

/**
 * \brief Get the exponent size of the given float type.
 */
size_t kratos_fp_type_get_exponent(kratos_env e, kratos_type t);

/**
 * \brief Get the mantissa size of the given float type.
 */
size_t kratos_fp_type_get_mantissa(kratos_env e, kratos_type t);

/**
 * \brief Get the index component of the given map type.
 * \return A borrowed reference.
 */
kratos_type kratos_map_type_get_index(kratos_env e, kratos_type t);

/**
 * \brief Get the element component of the given map type.
 * \return A borrowed reference.
 */
kratos_type kratos_map_type_get_element(kratos_env e, kratos_type t);

/**
 * \brief Get the number of arguments of the given function type.
 */
size_t kratos_function_type_get_num_args(kratos_env e, kratos_type t);

/**
 * \brief Get the number of return values of the given function type.
 */
size_t kratos_function_type_get_num_returns(kratos_env e, kratos_type t);

/**
 * \brief Get the array of arguments of the given function type.
 * \return An array of borrowed references (which must be deallocated
 *         with ::kratos_free()), or NULL on error
 */
kratos_type *kratos_function_type_get_args(kratos_env e, kratos_type t);

/**
 * \brief Get the array of return types of the given function type.
 * \return An array of borrowed references (which must be deallocated
 *         with ::kratos_free()), or NULL on error
 */
kratos_type *kratos_function_type_get_returns(kratos_env e, kratos_type t);

/**
 * \brief Get the number of values of the given enum type.
 */
size_t kratos_enum_type_get_num_values(kratos_env e, kratos_type t);

/**
 * \brief Get the array of values of the given enum type.
 * \return An array of borrowed references (which must be deallocated
 *         with ::kratos_free()), or NULL on error
 */
kratos_sexp *kratos_enum_type_get_values(kratos_env e, kratos_type t);

/**
 * \brief Checks whether the given symbol is a valid value
 *        for the given enum type.
 */
bool kratos_enum_type_is_value(kratos_env e, kratos_type t, kratos_sexp v);

/**
 * \brief Create a new void type.
 */
kratos_type kratos_void_type(kratos_env e);

/**
 * \brief Create a new label type.
 */
kratos_type kratos_label_type(kratos_env e);

/**
 * \brief Create a new int type.
 */
kratos_type kratos_int_type(kratos_env e);

/**
 * \brief Create a new real type.
 */
kratos_type kratos_real_type(kratos_env e);

/**
 * \brief Create a new symbolc type.
 */
kratos_type kratos_symbol_type(kratos_env e, kratos_sexp name);

/**
 * \brief Create a new bit-vector type.
 */
kratos_type kratos_bv_type(kratos_env e, size_t bits, bool is_signed);

/**
 * \brief Create a new float type type.
 */
kratos_type kratos_fp_type(kratos_env e, size_t exponent, size_t mantissa);

/**
 * \brief Create a new map type.
 */
kratos_type kratos_map_type(kratos_env e, kratos_type index,
                            kratos_type element);

/**
 * \brief Create a new function type.
 */
kratos_type kratos_function_type(kratos_env e, size_t num_args,
                                 kratos_type *args,
                                 size_t num_ret, kratos_type *ret);

/**
 * \brief Create a new enum type.
 */
kratos_type kratos_enum_type(kratos_env e, size_t num_values,
                             kratos_sexp *values);

/*@}*/ /* end of types group */

/**
 * \name Expressions creation and manipulation.
 */
/*@{*/

/**
 * \brief Increase the refcount of the given expression.
 * \return false in case of errors.
 */
bool kratos_expr_ref(kratos_env e, kratos_expr x);

/**
 * \brief Decrease the refcount of the given expression.
 * \return false in case of errors.
 */
bool kratos_expr_unref(kratos_env e, kratos_expr x);

/**
 * \brief Get the tag of the given expression.
 */
kratos_expr_tag kratos_expr_get_tag(kratos_env e, kratos_expr x);

/**
 * \brief Get the type of the given expression.
 * \return A borrowed reference.
 */
kratos_type kratos_expr_get_type(kratos_env e, kratos_expr x);

/**
 * \brief Get the var name of the given var expression.
 * \return A borrowed reference.
 */
kratos_sexp kratos_var_expr_get_var(kratos_env e, kratos_expr x);

/**
 * \brief Get the value of the given const expression.
 * \return A borrowed reference.
 */
kratos_sexp kratos_const_expr_get_value(kratos_env e, kratos_expr x);

/**
 * \brief Get op tag of the given op expression.
 */
kratos_op_tag kratos_op_expr_get_tag(kratos_env e, kratos_expr x);

/**
 * \brief Get the number of arguments of the given op expression.
 */
size_t kratos_op_expr_get_num_args(kratos_env e, kratos_expr x);

/**
 * \brief Get the list of arguments of the given op expression.
 * \return A list of borrowed references, which must be deallocated
 *         with ::kratos_free(), or NULL on error.
 */
kratos_expr *kratos_op_expr_get_args(kratos_env e, kratos_expr x);

/**
 * \brief Get the function of the given call expression.
 * \return A borrowed reference.
 */
kratos_expr kratos_call_expr_get_function(kratos_env e, kratos_expr x);

/**
 * \brief Get the number of arguments of the given call expression.
 */
size_t kratos_call_expr_get_num_args(kratos_env e, kratos_expr x);

/**
 * \brief Get the list of arguments of the given call expression.
 * \return A list of borrowed references, which must be deallocated
 *         with ::kratos_free(), or NULL on error.
 */
kratos_expr *kratos_call_expr_get_args(kratos_env e, kratos_expr x);

/**
 * \brief Get the number of return vars of the given call expression.
 */
size_t kratos_call_expr_get_num_returns(kratos_env e, kratos_expr x);

/**
 * \brief Get the list of return vars of the given call expression.
 * \return A list of borrowed references, which must be deallocated
 *         with ::kratos_free(), or NULL on error.
 */
kratos_expr *kratos_call_expr_get_returns(kratos_env e, kratos_expr x);

/**
 * \brief Get the var that is the lhs of the given assign expression.
 * \return A borrwed reference.
 */
kratos_expr kratos_assign_expr_get_var(kratos_env e, kratos_expr x);

/**
 * \brief Get the value that is the rhs of the given assign expression.
 * \return A borrwed reference.
 */
kratos_expr kratos_assign_expr_get_value(kratos_env e, kratos_expr x);

/**
 * \brief Get the condition of the given assume expression.
 * \return A borrowed reference.
 */
kratos_expr kratos_assume_expr_get_cond(kratos_env e, kratos_expr x);

/**
 * \brief Get the child expression of the given typecast expression.
 * \return A borrowed reference.
 */
kratos_expr kratos_typecast_expr_get_expr(kratos_env e, kratos_expr x);

/**
 * \brief Check whether the given typecast expression is a bitcast.
 */
bool kratos_typecast_expr_is_bitcast(kratos_env e, kratos_expr x);

/**
 * \brief Get the number of arguments of the given seq expression.
 */
size_t kratos_seq_expr_get_num_args(kratos_env e, kratos_expr x);

/**
 * \brief Get the list of arguments of the given seq expression.
 * \return A list of borrowed references, which must be deallocated
 *         with ::kratos_free(), or NULL on error.
 */
kratos_expr *kratos_seq_expr_get_args(kratos_env e, kratos_expr x);

/**
 * \brief Get the number of arguments of the given jump expression.
 */
size_t kratos_jump_expr_get_num_args(kratos_env e, kratos_expr x);

/**
 * \brief Get the list of arguments of the given jump expression.
 * \return A list of borrowed references, which must be deallocated
 *         with ::kratos_free(), or NULL on error.
 */
kratos_expr *kratos_jump_expr_get_args(kratos_env e, kratos_expr x);

/**
 * \brief Get the name of the given label expression.
 * \return A borrowed reference.
 */
kratos_sexp kratos_label_expr_get_name(kratos_env e, kratos_expr x);

/**
 * \brief Get the target of the given havoc expression.
 * \return A borrowed reference.
 */
kratos_expr kratos_havoc_expr_get_var(kratos_env e, kratos_expr x);

/**
 * \brief Get the condition of the given condjump expression.
 * \return A borrowed reference.
 */
kratos_expr kratos_condjump_expr_get_cond(kratos_env e, kratos_expr x);

/**
 * \brief Get the target of the given condjump expression.
 * \return A borrowed reference.
 */
kratos_expr kratos_condjump_expr_get_target(kratos_env e, kratos_expr x);

/**
 * \brief Create a new var expression.
 * \param e The environment in which to operate.
 * \param t The type of the expression.
 * \param n The name of the variable.
 * \return A new reference.
 */
kratos_expr kratos_var(kratos_env e, kratos_type t, kratos_sexp n);

/**
 * \brief Create a new const expression.
 * \param e The environment in which to operate.
 * \param t The type of the expression.
 * \param v The value of the constant.
 * \return A new reference.
 */
kratos_expr kratos_const(kratos_env e, kratos_type t, kratos_sexp v);

/**
 * \brief Create a new op expression.
 * \param e The environment in which to operate.
 * \param t The op tag.
 * \param num_args The number of arguments.
 * \param args The op arguments.
 * \return A new reference.
 */
kratos_expr kratos_op(kratos_env e, kratos_op_tag t,
                      size_t num_args, kratos_expr *args);

/**
 * \brief Create a new call expression.
 * \param e The environment in which to operate.
 * \param f The function to call.
 * \param num_args The number of arguments.
 * \param args The call arguments.
 * \param num_ret The number of returns.
 * \param ret The call return vars.
 * \return A new reference.
 */
kratos_expr kratos_call(kratos_env e, kratos_expr f,
                        size_t num_args, kratos_expr *args,
                        size_t num_ret, kratos_expr *ret);

/**
 * \brief Create a new assign expression.
 * \param e The environment in which to operate.
 * \param l The var to assign to.
 * \param r The value to assign.
 * \return A new reference.
 */
kratos_expr kratos_assign(kratos_env e, kratos_expr l, kratos_expr r);

/**
 * \brief Create a new assume expression.
 * \param e The environment in which to operate.
 * \param c The condition to assume.
 * \return A new reference.
 */
kratos_expr kratos_assume(kratos_env e, kratos_expr c);

/**
 * \brief Create a new assume expression.
 * \param e The environment in which to operate.
 * \param c The condition to assume.
 * \return A new reference.
 */
kratos_expr kratos_typecast(kratos_env e, kratos_type t, kratos_expr x,
                            bool is_bitcast);

/**
 * \brief Create a new sequence expression.
 * \param e The environment in which to operate.
 * \param num_args The number of arguments.
 * \param args The arguments.
 * \return A new reference.
 */
kratos_expr kratos_seq(kratos_env e, size_t num_args, kratos_expr *args);

/**
 * \brief Create a new sequence expression.
 * \param e The environment in which to operate.
 * \param num_args The number of arguments.
 * \param args The arguments (must be labels).
 * \return A new reference.
 */
kratos_expr kratos_jump(kratos_env e, size_t num_args, kratos_expr *args);

/**
 * \brief Create a new label expression.
 * \param e The environment in which to operate.
 * \param n The name of the label.
 * \return A new reference.
 */
kratos_expr kratos_label(kratos_env e, kratos_sexp n);

/**
 * \brief Create a new havoc expression.
 * \param e The environment in which to operate.
 * \param v The var to havoc.
 * \return A new reference.
 */
kratos_expr kratos_havoc(kratos_env e, kratos_expr v);

/**
 * \brief Create a new condjump expression.
 * \param e The environment in which to operate.
 * \param c The condition for the jump.
 * \param t The target label.
 * \return A new reference.
 */
kratos_expr kratos_condjump(kratos_env e, kratos_expr c, kratos_expr t);

/**
 * \brief Set the annotation for the given expression.
 * \param e The environment in which to operate.
 * \param x The expression to annotate.
 * \param a The annotation. Must be a list of cons cells
 *          of the form (key . value)
 * \return false on error.
 */
bool kratos_expr_set_annotation(kratos_env e, kratos_expr x, kratos_sexp a);

/**
 * \brief Get the annotation for the given expression.
 * \param e The environment in which to operate.
 * \param x An expression.
 * \return A new reference to the annotation for \a x,
 *         in the form of a list of (key . value) cons-cells.
 *         If \a x has no annotation, an error object is returned.
 */
kratos_sexp kratos_expr_get_annotation(kratos_env e, kratos_expr x);

/**
 * \brief visits the given expression  \a x, calling the callback \a cb
 * for every subexpression.
 *
 * \param e The environment in which to operate.
 * \param x The expression to visit.
 * \param cb The callback function.
 * \param user_data Generic data pointer which will be passed to \a cb. Can
 *                  be anything, its value will not be interpreted.
 * \return false on error.
 */ 
bool kratos_visit_expr(kratos_env e, kratos_expr x, kratos_visit_callback cb,
                       void *user_data);

/*@}*/ /* end of expr group */

/**
 * \name Programs creation and manipulation
 */
/*@{*/

/**
 * \brief Create a new function.
 * \param e The environment on which to operate.
 * \param name The name of the function.
 * \param num_params The number of formal parameters.
 * \param params The formal parameters (must be var expressions).
 * \param num_ret The number of return vars.
 * \param ret The return vars (must be var expressions).
 * \param num_locals The number of local variables.
 * \param locals The local variables (must be var expressions).
 * \param body The function body.
 * \return The new function created.
 *         Use ::KRATOS_ERROR_OBJ() to check for errors.
 */
kratos_function kratos_new_function(kratos_env e, kratos_sexp name,
                                    size_t num_params, kratos_expr *params,
                                    size_t num_ret, kratos_expr *ret,
                                    size_t num_locals, kratos_expr *locals,
                                    kratos_expr body);

/**
 * \brief Decrease the refcount of the given function.
 * \return false on error.
 */
bool kratos_function_unref(kratos_env e, kratos_function f);

/**
 * \brief Get the number of parameters of the given function.
 */
size_t kratos_function_get_num_params(kratos_env e, kratos_function f);

/**
 * \brief Get the number of local vars of the given function.
 */
size_t kratos_function_get_num_locals(kratos_env e, kratos_function f);

/**
 * \brief Get the number of return vars of the given function.
 */
size_t kratos_function_get_num_returns(kratos_env e, kratos_function f);

/**
 * \brief Get the name of the given function.
 * \return A borrowed reference.
 */
kratos_sexp kratos_function_get_name(kratos_env e, kratos_function f);

/**
 * \brief Get the array of parameters of the given function.
 * \return An array of borrwed references,
 *         which must be deallocated with ::kratos_free(), or NULL on error.
 */
kratos_expr *kratos_function_get_params(kratos_env e, kratos_function f);

/**
 * \brief Get the array of local vars of the given function.
 * \return An array of borrwed references,
 *         which must be deallocated with ::kratos_free(), or NULL on error.
 */
kratos_expr *kratos_function_get_locals(kratos_env e, kratos_function f);

/**
 * \brief Get the array of return vars of the given function.
 * \return An array of borrwed references,
 *         which must be deallocated with ::kratos_free(), or NULL on error.
 */
kratos_expr *kratos_function_get_returns(kratos_env e, kratos_function f);

/**
 * \brief Get the body of the given function.
 * \return A borrowed reference.
 */
kratos_expr kratos_function_get_body(kratos_env e, kratos_function f);

/**
 * \brief Get the type of the given function.
 * \return A borrowed reference.
 */
kratos_type kratos_function_get_type(kratos_env e, kratos_function f);

/**
 * \brief Set the annotation for the given function.
 *
 * The annotation must be a list of (key, value) pairs.
 * \return false on error.
 */
bool kratos_function_set_annotation(kratos_env e, kratos_function f,
                                    kratos_sexp a);

/**
 * \brief Get the annotation for the given function.
 * \return A new reference. If the function has no annotation,
 *         an error object will be returned.
 */
kratos_sexp kratos_function_get_annotation(kratos_env e, kratos_function f);

/**
 * \brief Create a new program.
 * \param e The environment in which to operate.
 * \param entrypoint The name of the entry point.
 * \param num_globals The number of global vars.
 * \param globals The list of global vars (must be var expressions).
 * \param num_funcs The number of functions.
 * \param funcs The list of functions.
 * \param init The init expression (can be an error object if not needed).
 * \return A new program.
 */
kratos_program kratos_new_program(kratos_env e, kratos_sexp entrypoint,
                                  size_t num_globals, kratos_expr *globals,
                                  size_t num_funcs, kratos_function *funcs,
                                  kratos_expr init);

/**
 * \brief Decrease the refcount for the given program.
 * \return false on error.
 */
bool kratos_program_unref(kratos_env e, kratos_program p);

/**
 * \brief Get the number of functions in the given program.
 */
size_t kratos_program_get_num_functions(kratos_env e, kratos_program p);

/**
 * \brief Get the list of functions in the given program.
 * \return An array of references, which must be deallocated
 *         with ::kratos_free(), or NULL on error.
 */
kratos_function *kratos_program_get_functions(kratos_env e, kratos_program p);

/**
 * \brief Get the init expression for the given program.
 * \return A borrowed reference.
 */
kratos_expr kratos_program_get_init(kratos_env e, kratos_program p);

/**
 * \brief Get the number of global vars in the given program.
 */
size_t kratos_program_get_num_globals(kratos_env e, kratos_program p);

/**
 * \brief Get the list of global vars of the given program.
 * \return An array of borrowed references, which must be deallocated
 *         with ::kratos_free(), or NULL on error.
 */
kratos_expr *kratos_program_get_globals(kratos_env e, kratos_program p);

/**
 * \brief Get the entry point of the given program.
 * \return A borrowed reference.
 */
kratos_sexp kratos_program_get_entry_point(kratos_env e, kratos_program p);

/*@}*/ /* end of programs group */

/**
 * \name Parsing and printing.
 */
/*@{*/

/**
 * \brief Parse a type from the given string.
 * \return A new reference.
 */
kratos_type kratos_parse_type(kratos_env e, const char *s);

/**
 * \brief Parse an expression from the given string.
 * \return A new reference.
 */
kratos_expr kratos_parse_expr(kratos_env e, const char *s);

/**
 * \brief Parse a program from the given string.
 * \return A new reference.
 */
kratos_program kratos_parse_program(kratos_env e, const char *s);

/**
 * \brief Get the string representation of the given type.
 * \return A dynamically allocated string, which must be deallocated
 *         with ::kratos_free(), or NULL on error.
 */
char *kratos_print_type(kratos_env e, kratos_type t);

/**
 * \brief Get the string representation of the given expression.
 * \return A dynamically allocated string, which must be deallocated
 *         with ::kratos_free(), or NULL on error.
 */
char *kratos_print_expr(kratos_env e, kratos_expr x);

/**
 * \brief Get the string representation of the given program.
 * \return A dynamically allocated string, which must be deallocated
 *         with ::kratos_free(), or NULL on error.
 */
char *kratos_print_program(kratos_env e, kratos_program p);

/*@}*/ /* end of parsing group */

/**
 * Verification.
 */
/*@{*/

/**
 * \brief Create a new configuration for verification.
 */
kratos_config kratos_new_config(kratos_env e);

/**
 * \brief Set an option for the given configuration.
 * \return false on error.
 */
bool kratos_config_set_option(kratos_env e, kratos_config c,
                              const char *opt, const char *val);

bool kratos_config_update(kratos_env e, kratos_config c, const char *filename);

/**
 * \brief Decrease the refcount of the given configuration.
 * \return false on error.
 */
bool kratos_config_unref(kratos_env e, kratos_config c);

/**
 * \brief Set the custom constraint provider for symbolic execution.
 * See ::kratos_symexec_constraint_provider.
 * \return false on error.
 */
bool kratos_config_set_symexec_constraint_provider(
    kratos_env e, kratos_config c,
    kratos_symexec_constraint_provider f, void *data);

/**
 * \brief Set the custom object monitor for symbolic execution.
 * See ::kratos_symexec_object_monitor.
 * \return false on error.
 */
bool kratos_config_set_symexec_object_monitor(
    kratos_env e, kratos_config c,
    kratos_symexec_object_monitor f, void *data);

/**
 * \brief Perform verification of the given program.
 * \param e The environment in which to operate.
 * \param method The verification method to use.
 * \param conf The configuration to use.
 * \param p The program to verify.
 * \param out If not-NULL, pointer to a trace to store the counterexample
 *            (if any). Note that counterexample generation must be enabled
 *            in the configuration.
 * \return The verification result, or ::KRATOS_RESULT_ERROR on error.
 */
kratos_result kratos_verify(kratos_env e, kratos_verification_method method,
                            kratos_config conf,
                            kratos_program p, kratos_trace *out);

/**
 * \brief Encode the given program into a symbolic transition system.
 * \param e The environment in which to operate.
 * \param conf The configuration to use.
 * \param p The program to encode.
 * \return A string representation of the resulting transition system,
 *         which must be deallocated with ::kratos_free(), or NULL on error.
 */
char *kratos_encode(kratos_env e, kratos_config conf, kratos_program p);

/**
 * \brief Get the search statistics of the latest verification run.
 * \param e The environment in which to operate.
 * \return The string of statistics, which must be deallocated
 *         with ::kratos_free(), or NULL on error.
 */
char *kratos_get_stats(kratos_env e);

/**
 * \brief Test whether the given trace is a path.
 */
bool kratos_trace_is_path(kratos_env e, kratos_trace t);

/**
 * \brief Get the length of the given path trace.
 */
size_t kratos_trace_get_path_length(kratos_env e, kratos_trace t);

/**
 * \brief Get the path of the given path trace.
 * \return A list of borrowed references, which must be deallocated
 *         with ::kratos_free(), or NULL on error.
 */
kratos_trace *kratos_trace_get_path(kratos_env e, kratos_trace t);

/**
 * \brief Get the step expression of the given trace.
 * \return A borrowed reference.
 */
kratos_expr kratos_trace_get_step(kratos_env e, kratos_trace t);

/**
 * \brief Decrease the refcount of the given trace.
 * \return false on error.
 */
bool kratos_trace_unref(kratos_env e, kratos_trace t);

/*@}*/ /* end of verification group */

#ifdef __cplusplus
} /* end of extern "C" */
#endif

#endif /* KRATOS_H_INCLUDED */
