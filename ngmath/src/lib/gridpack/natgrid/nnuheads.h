#include <stdio.h>
#include <string.h>

#define EQ             ==
#define NE             !=
#define AND            &&
#define OR             ||

extern int       igrad,  densi,   non_neg,    sdip,        rads,
                 optim,  extrap,  southhemi,  updir, auto_scale,
                 adf,    dup;

extern double    bI,        bJ,         magx,       magy,
                 magz,      horilap,    vertlap,    nuldat,
                 magx_auto, magy_auto,  magz_auto,  horilap_save,
                 vertlap_save;

extern char      tri_file[], error_file[], emsg[];
extern FILE      *filee;

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

void   c_nnsetc(char *, char *);
void   c_nngetc(char *, char *);
void   c_nnseti(char *, int);
void   c_nngeti(char *, int *);
void   c_nnsetr(char *, float);
void   c_nngetr(char *, float *);

extern void   c_nngetslopes(int, int, float *, int *);
extern void   c_nngetaspects(int, int, float *, int *);
extern void   c_nnpntinits(int, float *, float *, float *);
extern void   c_nnpnts(float, float, float *);
extern void   c_nnpntend();
extern void   ErrorHnd(int, char *, FILE *, char *);

/*
 *  Fortran entry points.
 */
void  NGCALLF(natgrids,NATGRIDS) (int *, float *, float *, float *,
              int *, int *, float *, float *, float *, int *);
void  NGCALLF(nnseti,NNSETI) (char *, int *);
void  NGCALLF(nngeti,NNGETI) (char *, int *);
void  NGCALLF(nnsetr,NNSETR) (char *, float *);
void  NGCALLF(nngetr,NNGETR) (char *, float *);
void  NGCALLF(f_nnsetc,F_NNSETC) (char *, char *, int *);
void  NGCALLF(f_nngetc,F_NNGETC) (char *, char *, int *);
void  NGCALLF(nngetslopes,NNGETSLOPES) (int *, int *, float *, int *);
void  NGCALLF(nngetaspects,NNGETASPECTS) (int *, int *, float *, int *);
void  NGCALLF(nnpntinits,NNPNTINITS) (int *, float *, float *, float *);
void  NGCALLF(nnpnts,NNPNTS) (float *, float *, float *);
void  NGCALLF(nnpntend,NNPNTEND) ();


float  **c_natgrids(int, float [], float [], float [],
                    int, int, float [], float [], int *);
