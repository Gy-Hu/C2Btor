#pragma once

extern void __VERIFIER_assume(_Bool expr);
extern void __VERIFIER_error();

#define assume __VERIFIER_assume

int sassert(_Bool expr)
{
    if (!expr) {
      ERROR: __VERIFIER_error();
        return 0;
    }
    return 1;
}

