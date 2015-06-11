#include "Test.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include <fcntl.h>

#if darwin_HOST_OS
int xox(int filedes, off_t fst_offset, off_t fst_length) {
    fstore_t bar;
    bar.fst_flags = F_ALLOCATEALL;
    bar.fst_posmode = F_PEOFPOSMODE;
    bar.fst_offset = fst_offset;
    bar.fst_length = fst_length;
    return (fcntl(filedes, F_PREALLOCATE, &bar));
}
#endif
