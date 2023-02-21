#include <time.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

char* base = "abcdefghijklmnopqrstuvwxyz123456";

double now() {
    char time_str[32];
    struct timespec spec;
    clock_gettime(CLOCK_REALTIME, &spec);
    sprintf(time_str, "%ld.%.9ld", spec.tv_sec, spec.tv_nsec);
    return atof(time_str);
}

int main(int argc, char* argv[]) {
    char* str = malloc(strlen(base) * 1000000 + strlen("waldo") + 1);
    for (int i = 0; i < 1000000; i += 1) {
        memcpy(str + i * strlen(base), base, strlen(base));
    }
    memcpy(str + 1000000 * strlen(base), "waldo", strlen("waldo"));
    str[1000000 * strlen(base) + strlen("waldo")] = 0;

    double start = now();
    int idx = 0;
    for (int i = 0; i < 100; i += 1) {
        char* res = strstr(str, "waldo");
        idx = res - str;
    }
    printf("idx: %d ms: %f\n", idx, now() - start);
}