#include "Test.h"


int __xox(int filedes, u_int32_t fst_flags, int fst_posmode, off_t fst_offset, off_t fst_length, off_t fst_bytesalloc) {
    fstore_t bar;
    bar.fst_flags = fst_flags;
    bar.fst_posmode = fst_posmode;
    bar.fst_offset = fst_offset;
    bar.fst_length = fst_length;
    bar.fst_bytesalloc = fst_bytesalloc;
    return (fcntl(filedes, 42, bar));
}
