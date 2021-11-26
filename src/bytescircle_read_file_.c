//
// bytescircle
//
// by Roberto S. Galende
// port of linux' bytes-circle to R
// v1.0-1, Nov 2016
//
// licensed under GPL-3
//

// LFS (Large File Support):
#define _FILE_OFFSET_BITS 64    // stat, fseek
#define _LARGEFILE_SOURCE
#define _LARGEFILE64_SOURCE     // off64_t for fseek64

#define BUFFER_LENGTH 4096

#include <stdio.h>  // FILE, fopen
#include <string.h> // strlen
#include "bytescircle_read_file_.h"

void bytescircle_read_file_(char** R_FILE, double* R_BYTE_out) {
    
    const unsigned int buffer_length = BUFFER_LENGTH;
    long long bytes[256];
    int i;
    FILE *hFile;
    ////long long total_size = 0;
    char buffer[BUFFER_LENGTH];
    size_t k;
    size_t bytes_read;

    if (strlen(R_FILE[0])==0) {
        // return no result
        return;
    } else {
        hFile = fopen(R_FILE[0], "rb");
    }

    if ( hFile == NULL ) {
        return;
    }

    // fill counter matrix with zeros
    for (i = 0; i < 256; ++i)
        bytes[i] = 0;

    // actually count different bytes in file
    do {
        bytes_read = fread(buffer, 1, buffer_length, hFile);
        for (k = 0; k < bytes_read; ++k)
            ++bytes[(unsigned char)buffer[k]];
        ////total_size += bytes_read;
    } while (bytes_read == buffer_length);

    // close file: it is not needed any more
    fclose (hFile);

    // as R does not have "long long" type, 
    // exact values must be converted to double
    // (lost of accuracy can be up to 64-53=11 (least significant) bits,
    // but this should be irrevelant for this statistic work,
    // as relative magnitudes are preserved)
    for (i = 0; i < 256; ++i) {
        R_BYTE_out[i] = (double)bytes[i];
    }

}
