/* SCCS @(#)	1.6 02/24/00 */
/*
 **   The S.h file defines a few things that I need, and hundreds that I don't.
 ** In particular, on some architectures, it defines a variable "time"
 ** which of course conflicts with lots of my C-code, 'time' being a natural
 ** variable name for survival models and thus used in the poisson routines.
 **   Thanks to Brian Ripley for suggesting a machine independent way of
 ** fixing this.
 **
 ** The S_alloc function changed it's argument list from version 4 to 5, and
 **   the ALLOC macro allows me to have common C code for the two versions,
 **   with only this file "survS.h" changed.
 */
#define time timexxx
#include "R.h"
#undef time
#undef error

/*
 ** Memory defined with S_alloc is removed automatically
 **  That with "CALLOC" I have to remove myself.  Use the
 **  latter for objects that need to persist between the
 **  s_to_ht1 and s_to_ht2 calls
 */
/*
 R_alloc(long n, int size);  # memory is not zeroed
 S_alloc(long n, int size);  # memory is zeroed
 */

#define ALLOC(a,b)  S_alloc(a,b)
//#define ALLOC(a,b)  R_alloc(a,b)
#define CALLOC(a,b) R_chk_calloc((size_t)(a), b)
#define FREE(p)      (R_chk_free( (void *)(p) ), (p) = NULL)

#ifndef FLOAT
#define FLOAT double    /* see hurrey.h */
#endif

/*
#ifndef z
#define Sint int
#endif
 */

#ifndef z
#define SEXP int
#endif
