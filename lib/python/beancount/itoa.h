#ifndef __ITOA_H__
#define __ITOA_H__

#include <cstdint>
#include <cstdlib>

// Convert a fixed precision integer (10^9) to a string with the given precision.
size_t itoall(int64_t value, char* result, int alignment, unsigned int base = 10, char fill = '0');

#endif
