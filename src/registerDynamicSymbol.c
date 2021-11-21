// RegisteringDynamic Symbols

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>
#include "bytescircle_read_file_.h"

void R_init_bytescircle(DllInfo* info) {

    static R_NativePrimitiveArgType bytescircle_read_file__type[] = {
        STRSXP, REALSXP
    };

    static const R_CMethodDef cMethods[] = {
       {"bytescircle_read_file_", (DL_FUNC) &bytescircle_read_file_, 2, bytescircle_read_file__type},
       {NULL, NULL, 0, NULL}
    };

    R_registerRoutines(info, cMethods, NULL, NULL, NULL);

    R_useDynamicSymbols(info, TRUE);

}
