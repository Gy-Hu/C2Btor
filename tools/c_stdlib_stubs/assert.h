#pragma once

void __VERIFIER_error();

/* */
void KRATOS_ASSERT(_Bool expr)
{
    if (!expr) {
      ERROR: __VERIFIER_error(); goto ERROR;
    }
}

#ifdef KRATOS_ASSERT_CHECK
#  define assert(e) KRATOS_ASSERT(e)
#else
#  define assert(e)
#endif
