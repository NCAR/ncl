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
void   c_nnsetrd(char *, double);
void   c_nngetrd(char *, double *);

extern void   c_nngetsloped(int, int, double *, int *);
extern void   c_nngetaspectd(int, int, double *, int *);
extern void   c_nnpntinitd(int, double *, double *, double *);
extern void   c_nnpntd(double, double, double *);
extern void   c_nnpntendd();
extern void   ErrorHnd(int, char *, FILE *, char *);

/*
 *  Fortran entry points.
 */
void  NGCALLF(natgridd,NATGRIDD) (int *, double *, double *, double *,
              int *, int *, double *, double *, double *, int *);
void  NGCALLF(nnseti,NNSETI) (char *, int *);
void  NGCALLF(nngeti,NNGETI) (char *, int *);
void  NGCALLF(nnsetrd,NNSETRD) (char *, double *);
void  NGCALLF(nngetrd,NNGETRD) (char *, double *);
void  NGCALLF(f_nnsetc,F_NNSETC) (char *, char *, int *);
void  NGCALLF(f_nngetc,F_NNGETC) (char *, char *, int *);
void  NGCALLF(nngetsloped,NNGETSLOPED) (int *, int *, double *, int *);
void  NGCALLF(nngetaspectd,NNGETASPECTD) (int *, int *, double *, int *);
void  NGCALLF(nnpntinitd,NNPNTINITD) (int *, double *, double *, double *);
void  NGCALLF(nnpntd,NNPNTD) (double *, double *, double *);
void  NGCALLF(nnpntend,NNPNTEND) ();


double  **c_natgridd(int, double [], double [], double [],
                    int, int, double [], double [], int *);
