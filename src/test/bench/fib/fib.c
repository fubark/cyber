#include <stdio.h>
#include <time.h>

int fib(int n) {
    if (n < 2) {
        return n;
    }
    return fib(n-1) + fib(n-2);
}

int main() {
    float startTime = (float)clock()/CLOCKS_PER_SEC;
    int res = fib(30);
    float endTime = (float)clock()/CLOCKS_PER_SEC;
    printf("time: %f\n", (endTime - startTime) * 1000);
    printf("%d\n", res);
    return 0;
}
