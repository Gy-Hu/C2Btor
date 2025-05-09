#include <stdint.h>
#include <stdio.h>

// SV-COMP verification function
extern void __VERIFIER_error();

void divide(uint32_t dividend, uint32_t divisor, uint32_t *quotient, uint32_t *remainder) {
    if (divisor == 0) {
        printf("Error: Division by zero.\n");
        *quotient = 0;  
        *remainder = 0; 
        return;
    }

    if(dividend == 0){
        *quotient = 0;
        *remainder = 0;
        return;
    }

    // Perform division and modulus
    *quotient = dividend / divisor;
    *remainder = dividend % divisor;
}

// int unknown(){
//     return 0;
// }

int main() {
    uint32_t dividend;  
    uint32_t divisor;   
    uint32_t quotient = 0;
    uint32_t remainder = 0;

    divide(dividend, divisor, &quotient, &remainder);
    printf("Quotient: %u\n", quotient);
    printf("Remainder: %u\n", remainder);

    if (!(quotient == 1 || remainder == 1)) {
        __VERIFIER_error();
    }
    
    return 0;
}
