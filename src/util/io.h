#ifndef IO_H
#define IO_H

#include <stdio.h>
#include <inttypes.h>

typedef FILE IO;

/**
 * The schema for this utility methods is
 *
 * <prefix>i/o_<type>_<endian>
 *
 * ioo_byte outputs a byte
 * ioi_int_le inputs a 32 bit integer little endian
 */

void ioo_byte(IO * io, char byte);
char ioi_byte(IO * io);

void ioo_int_le(IO * io, int32_t integer);
int32_t ioi_int_le(IO * io);

#endif
