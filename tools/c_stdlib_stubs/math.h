#pragma once
#include <assert.h>

/* */

#define MAXFLOAT	3.40282347e+38F
/* Some useful constants.  */
#define M_E		2.7182818284590452354	/* e */
#define M_LOG2E	1.4426950408889634074	/* log_2 e */
#define M_LOG10E	0.43429448190325182765	/* log_10 e */
#define M_LN2		0.69314718055994530942	/* log_e 2 */
#define M_LN10		2.30258509299404568402	/* log_e 10 */
#define M_PI		3.14159265358979323846	/* pi */
#define M_PI_2		1.57079632679489661923	/* pi/2 */
#define M_PI_4		0.78539816339744830962	/* pi/4 */
#define M_1_PI		0.31830988618379067154	/* 1/pi */
#define M_2_PI		0.63661977236758134308	/* 2/pi */
#define M_2_SQRTPI	1.12837916709551257390	/* 2/sqrt(pi) */
#define M_SQRT2	        1.41421356237309504880	/* sqrt(2) */
#define M_SQRT1_2	0.70710678118654752440	/* 1/sqrt(2) */

#define INFINITY        (1.0/0.0)
#define NAN             (0.0/0.0)
#define HUGE_VAL        INFINITY                /* signals overflow */
#define HUGE_VALF       INFINITY                /* signals overflow */
#define HUGE_VALL       INFINITY                /* signals overflow */

#define	FP_INFINITE	0
#define	FP_NAN		1
#define	FP_NORMAL	2
#define	FP_SUBNORMAL	3
#define	FP_ZERO		4

double fabs(double x)
{
    if (x < 0.0) {
        return -x;
    } else {
        return x;
    }
}

#define isunordered(x, y) (isnan(x) || isnan(y))
#define islessgreater(x, y) ((x) < (y) || (x) > (y))
#define isgreater(x, y) ((x) > (y))
#define isgreaterequal(x, y) ((x) >= (y))
#define isless(x, y) ((x) < (y))
#define islessequal(x, y) ((x) <= (y))
#define fdim(x, y) fmax((x) - (y), 0.0)
#define nan(x) NAN
#define nanf(x) NAN
#define nanl(x) NAN
#define fmin(x, y) ((x) < (y) ? (x) : (y))
#define fmax(x, y) ((x) > (y) ? (x) : (y))

double ceil(double x)
{
    return (int)(x + 0.5);
}

int abs(int x)
{
    if (x < 0) {
        return -x;
    } else {
        return x;
    }
}

// inspired by https://codebrowser.dev/gcc/include/math.h.html
#define fpclassify(x) \
	((sizeof (x) == sizeof (float)) ? \
		__fpclassifyf(x) \
	: (sizeof (x) == sizeof (double)) ? \
		__fpclassify(x) \
	:	__fpclassifyl(x))

#ifndef KRATOS_PRECISE_MATH_H

double sqrt(double x)
{
    double ret;
    return ret;
}


double asin(double x)
{
    double ret;
    return ret;
}


double sin(double x)
{
    double ret;
    return ret;
}


double cos(double x)
{
    double ret;
    return ret;
}


double tan(double x)
{
    double ret;
    return ret;
}


int __fpclassify(double x)
{
    int ret;
    return ret;
}


int __fpclassifyf(float x)
{
    int ret;
    return ret;
}


int __fpclassifyl(long double x)
{
    int ret;
    return ret;
}

double rint(double x)
{
    return (double)((int)x);
}

double pow(double x, double y)
{
    double ret;
    return ret;
}

#else /* KRATOS_PRECISE_MATH_H */

double sqrt(double x)
{
    double res;
    if (x >= 0.0) {
        KRATOS_ASSUME(res * res == x);
        return res;
    } else {
        KRATOS_ASSUME(isnan(res));
        return res;
    }
}


//  this code was adapted from http://shibatch.sourceforge.net/
//
//   Copyright Naoki Shibata and contributors 2010 - 2021.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file sleef_LICENSE.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
#define mla(x, y, z) x * y + z
#define PI4_A .7853981554508209228515625
#define PI4_B .794662735614792836713604629039764404296875e-8
#define PI4_C .306161699786838294306516483068750264552437361480769e-16

double sin(double d)
{
    int q;
    double u, s;

    double x = d * M_1_PI;
    if (x < 0) {
        q = (int)(x - 0.5);
    } else {
        q = (int)(x + 0.5);
    }

    d = mla(q, -PI4_A*4, d);
    d = mla(q, -PI4_B*4, d);
    d = mla(q, -PI4_C*4, d);

    s = d * d;

    if ((q & 1) != 0) d = -d;

    u = -7.97255955009037868891952e-18;
    u = mla(u, s, 2.81009972710863200091251e-15);
    u = mla(u, s, -7.64712219118158833288484e-13);
    u = mla(u, s, 1.60590430605664501629054e-10);
    u = mla(u, s, -2.50521083763502045810755e-08);
    u = mla(u, s, 2.75573192239198747630416e-06);
    u = mla(u, s, -0.000198412698412696162806809);
    u = mla(u, s, 0.00833333333333332974823815);
    u = mla(u, s, -0.166666666666666657414808);

    u = mla(s, u * d, d);

    return u;
}

#undef ONE_OVER_PI
#undef PI4_C
#undef PI4_B
#undef PI4_A
#undef mla

double cos(double x)
{
    return sin(M_PI_2 - x);
}


double tan(double x)
{
    return sin(x)/cos(x);
}


double asin(double x)
{
    double ret;
    if (x >= -1.0 && x <= 1.0) {
        KRATOS_ASSUME(sin(ret) == x);
    }
    return ret;
}


#define __fpclassify_t(TYPE, NAME) \
    int NAME (TYPE x) { \
    if (isinf(x)) { return FP_INFINITE; } \
    if (isnan(x)) { return FP_NAN; } \
    if (isnormal(x)) { return FP_NORMAL; } \
    if (issubnormal(x)) { return FP_SUBNORMAL; } \
    return FP_ZERO; }

__fpclassify_t(double, __fpclassify)
__fpclassify_t(float, __fpclassifyf)
__fpclassify_t(long double, __fpclassifyl)


double rint(double x)
{
    return (double)((int)x);
}

#endif /* KRATOS_PRECISE_MATH_H */
