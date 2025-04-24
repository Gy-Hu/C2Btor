#include "assert.h"

#define LARGE_INT 1000

extern int __VERIFIER_nondet_int(void);
extern void __VERIFIER_error(void);

int main() {
    int i, pvlen;
    int tmp___1;
    int k = 0;
    int n;

    i = 0;
    pvlen = __VERIFIER_nondet_int();

    while (__VERIFIER_nondet_int() && i <= LARGE_INT) {
        i = i + 1;
    }

    if (i > pvlen) {
        pvlen = i;
    }
    i = 0;

    while (__VERIFIER_nondet_int() && i <= LARGE_INT) {
        tmp___1 = i;
        i = i + 1;
        k = k + 1;
    }

    int j = 0;
    n = i;
    while (1) {
        if (!(k >= 0)) {
            __VERIFIER_error();
        }
        k = k - 1;
        i = i - 1;
        j = j + 1;
        if (j >= n) {
            break;
        }
    }

    return 0;
}