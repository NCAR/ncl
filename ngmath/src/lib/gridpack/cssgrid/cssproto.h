#include <stdio.h>
 
/*
 *  Specify all of the function prototypes.
 */
int   *c_csstri(int, float [], float [], float [], int *, int *);
float *c_cssgrid(int, float [], float [], float [], float [],
                 int, int, float [], float [], int *);
void   c_cstrans(int, float *, float *, float *, float *, float *);


/*
 *  Fortran function macro.  This macro is used to provide the appropriate
 *  system-specific C function name for it to be Fortran callable.
 */
#ifndef NGCALLF
 
#ifdef  UNICOS
#define NGCALLF(reg,caps)       caps
 
#elif   defined(RS6000) || defined(__hpux)
#define NGCALLF(reg,caps)       reg
 
#else
#ifdef  __STDC__
#define NGCALLF(reg,caps)       reg##_
#else
#define NGCALLF(reg,caps)       reg/**/_
 
#endif  /* __STDC__ */
#endif  /* UNICOS else ... */
#endif  /* NGCALLF */

