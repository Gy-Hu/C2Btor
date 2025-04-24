#include <stdint.h>
extern int __VERIFIER_nondet_int();
extern _Bool __VERIFIER_nondet__Bool();
extern char __VERIFIER_nondet_char();
extern short __VERIFIER_nondet_short();
extern void __VERIFIER_error();
extern void abort(void);
extern void __assert_fail(const char *, const char *, unsigned int, const char *);
void reach_error() { __VERIFIER_error(); }
// function declarations
void rule_0();
void rule_1();
void rule_2();

// global variables
uint8_t state_0;
_Bool state;

uint8_t ___0;
uint8_t ___1;
_Bool __;

// rules
/*
ForAll([__chosen, chosen],
       Implies(__chosen == 0, state(__chosen)))
*/
void rule_0() {
  uint8_t chosen = __VERIFIER_nondet_char();
  uint8_t __chosen = __VERIFIER_nondet_char();
  if ((__chosen == 0U)) {
    state_0 = __chosen;
    state = 1;
  } else {
    abort();
  }
}


/*
ForAll([__chosen, chosen],
       Implies(And(state(__chosen),
                   Or(__chosen == chosen, chosen == 1)),
               state(chosen)))
*/
void rule_1() {
  uint8_t chosen = __VERIFIER_nondet_char();
  uint8_t __chosen = __VERIFIER_nondet_char();
  if (((state && state_0 == __chosen) && ((__chosen == chosen) || (chosen == 1U)))) {
    state_0 = chosen;
    state = 1;
  } else {
    abort();
  }
}


/*
ForAll([__chosen, chosen],
       Implies(state(__chosen), __chosen == 0))
*/
void rule_2() {
  uint8_t chosen = __VERIFIER_nondet_char();
  uint8_t __chosen = __VERIFIER_nondet_char();
  if ((state && state_0 == __chosen)) {
    ___0 = __chosen;
    ___1 = 0U;
    __ = 1;
    
    // Property: After rule_2 executes, __chosen must be 0
    // This should hold according to the rule's postcondition
    if (__chosen != 0U) {
      reach_error();
    }
  } else {
    abort();
  }
}



// main function
int main() {
  while(1) {
    switch(__VERIFIER_nondet_int()) {
      case 0: rule_0(); break;
      case 1: rule_1(); break;
      case 2: rule_2(); break;
      default: abort(); break;
    }
  }
}