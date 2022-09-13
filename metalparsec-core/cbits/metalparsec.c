#include <stddef.h>
#include <stdint.h>
#include <string.h>

int metalparsec_memcmp_off(const uint8_t *s1, size_t off1, const uint8_t *s2,
                           size_t off2, size_t n) {
    const uint8_t *s1o = s1 + off1;
    const uint8_t *s2o = s2 + off2;

    return memcmp(s1o, s2o, n);
}

size_t metalparsec_memchr_off(const uint8_t *s, size_t off, int b, size_t n) {
    const uint8_t *s_res = memchr(s + off, b, n);
    if (s_res == NULL) {
        return 0;
    } else {
        return s_res - s + 1;
    }
}
