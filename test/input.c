#include <stdio.h>
#include <assert.h>

int main() {
    int x = 5;
    int y = 0;
    
    if (x > 0) {
        y = 10;
    } else {
        y = -10;
    }
    
    assert(y > 0); // This assertion will be true
    
    return 0;
}