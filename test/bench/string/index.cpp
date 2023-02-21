#include <sstream>

double now() {
    char time_str[32];
    struct timespec spec;
    clock_gettime(CLOCK_REALTIME, &spec);
    sprintf(time_str, "%ld.%.9ld", spec.tv_sec, spec.tv_nsec);
    return atof(time_str);
}

int main(int argc, char* argv[]) {
    std::ostringstream os;
    for (int i = 0; i < 1000000; i += 1) {
        os << "abcdefghijklmnopqrstuvwxyz123456";
    }
    os << "waldo";
    std::string str = os.str();

    double start = now();
    int idx = 0;
    for (int i = 0; i < 100; i += 1) {
        idx = str.find("waldo");
    }
    printf("idx: %d ms: %f\n", idx, now() - start);
}