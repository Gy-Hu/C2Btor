#pragma once

#define __VERIFIER_nondet(TYPE) TYPE __VERIFIER_nondet_ ## TYPE () { TYPE ret; return ret; }

__VERIFIER_nondet(char)
__VERIFIER_nondet(int)
__VERIFIER_nondet(float)
__VERIFIER_nondet(double)
__VERIFIER_nondet(long)
__VERIFIER_nondet(short)

#define __builtin_nan(x) 0.0/0.0
#define __builtin_nanf(x) 0.0f/0.0f
#define __builtin_isnan(x) isnan(x)
#define __builtin_isinf(x) isinf(x)
#define __isnan(x) isnan(x)
#define __isnanf(x) isnan(x)
#define __isnanl(x) isnan(x)
#define __isinf(x) isinf(x)
#define __isinff(x) isinf(x)
#define __isinfl(x) isinf(x)
#define __builtin_isfinite(x) isfinite(x)
#define __finite(x) isfinite(x)
#define __finitef(x) isfinite(x)
#define __finitel(x) isfinite(x)

double nan(const char* x)
{
    return (double)0.0 / 0.0;
}

#define CHAR_BIT 8
#define SCHAR_MIN (-128)
#define SCHAR_MAX 127

#define UCHAR_MAX 255

#define CHAR_MIN SCHAR_MIN
#define CHAR_MAX SCHAR_MAX

#define SHRT_MIN (-32768)
#define SHRT_MAX 32767

#define USHRT_MAX 65535

#define INT_MIN	(-INT_MAX - 1)
#define INT_MAX	2147483647
#define UINT_MAX 4294967295U

int __VERIFIER_assume(int);
#define KRATOS_ASSUME __VERIFIER_assume

#include "stdlib.h"
#include "math.h"
