#ifndef PKG_TIFF_COMMON_H__
#define PKG_TIFF_COMMON_H__

#include <stdio.h>
#include <tiff.h>
#include <tiffio.h>

typedef struct tiff_job {
    FILE *f;  // the TIFF file
    long ptr, len, alloc;
    char *data;
} tiff_job_t;

TIFF *TIFF_Open(const char *mode, tiff_job_t *rj);

void check_type_sizes(void);

typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long long int uint64_t;
#endif
