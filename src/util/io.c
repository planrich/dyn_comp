#include "io.h"

void ioo_byte(IO * io, char byte) {
    fwrite(&byte, 1, 1, io);
}
char ioi_byte(IO * io) {
    char byte;
    fread(&byte, 1, 1, io);
    return byte;
}

void ioo_int_le(IO * io, int32_t integer) {
    fwrite(&integer, 4, 1, io);
}
int32_t ioi_int_le(IO * io) {
    int32_t integer;
    fread(&integer, 4, 1, io);
    return integer;
}
