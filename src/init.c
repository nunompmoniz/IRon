#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void r2phi(void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"r2phi", (DL_FUNC) &r2phi, 6},
    {NULL, NULL, 0}
};

void R_init_IRon(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
