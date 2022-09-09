#include <stddef.h>
#include <string.h>

int metalparsec_memcmp_off(const void *s1, size_t off1, const void *s2,
                           size_t off2, size_t n) {
    const void *s1o = s1 + off1;
    const void *s2o = s2 + off2;

    return memcmp(s1o, s2o, n);
}
