#include <stdio.h>

/*
 *  Specify all of the function prototypes.
 */

double   dist_pow(double);
float    dist_pow_s(float);
void     dweights(int);
void     dweights_s(int);
void     sweights(int, double, double, double);
void     sweights_s(int, float, float, float);
float    *c_dsgrid2s(int, float [], float [], float [],
                     int, int, float [], float [], int *);
double   *c_dsgrid2d(int, double [], double [], double [],
                     int, int, double [], double [], int *);
float    *c_dsgrid3s(int, float [], float [], float [], float [],
                     int, int, int, float [], float [], float [], int *);
double   *c_dsgrid3d(int, double [], double [], double [], double [],
                     int, int, int, double [], double [], double [], int *);
void     c_dspnt3d(int, double [], double [], double [], double [],
                   int, double [], double [], double [], double *, int *);
void     c_dspnt2d(int, double [], double [], double [],
                   int, double [], double [], double *, int *);
void     c_dspnt3s(int, float [], float [], float [], float [],
                   int, float [], float [], float [], float *, int *);
void     c_dspnt2s(int, float [], float [], float [],
                   int, float [], float [], float *, int *);
double   dotd(DSpointd3, DSpointd3);
float    dot_s(DSpoints3, DSpoints3);
double   ivalue(int, double *, double, double, double);
float    ivalue_s(int, float *, float, float, float);
double   svalue(int, double *, double, double, double);
float    svalue_s(int, float *, float, float, float);
void     dsinit(int, int, int, int, double [], double [], double [], 
                double[], double[], double[], int *);
void     dsinit_s(int, int, int, int, float [], float [], float [], 
                float[], float[], float[], int *);
void     dsgetmem(int, int, int, int, int *);
void     dsgetmem_s(int, int, int, int, int *);
void     dsfreemem();
void     dsfreemem_s();
void     dsdist(int, DSpointd3 [], DSpointd3, double *);
void     dsdist_s(int, DSpoints3 [], DSpoints3, float *);
void     dssortd(int, double [], int []);
void     dssorts(int, float [], int []);
double   dsangd (DSpointd3, DSpointd3, DSpointd3);
float    dsangs (DSpoints3, DSpoints3, DSpoints3);
double   magd (DSpointd3);
float    mags (DSpoints3);
void     DSErrorLog(int, char *, FILE *, char *);
void     DSErrorHnd(int, char *, FILE *, char *);
char     *DSErrMsg(int);
int      DSErrMax();
void     c_dssetc(char *, char *);
void     c_dsgetc(char *, char *);
void     c_dsseti(char *, int);
void     c_dsgeti(char *, int *);
void     c_dsgetr(char *, float *);
void     c_dssetr(char *, float);
void     c_dssetrd(char *, double);
void     c_dsgetrd(char *, double *);

#ifndef MAX
#define MAX(A,B)        (((A) > (B)) ? (A) : (B))
#endif

#ifndef MIN
#define MIN(A,B)        (((A) < (B)) ? (A) : (B))
#endif

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

/*
 *  Fortran entry points.
 */
void NGCALLF(dsgrid3d,DSGRID3D) (int *, double *, double *, double *,
                double *, int *, int *, int *,
                double *, double *, double *, double *, int *ier);
void NGCALLF(dsgrid2d,DSGRID2D) (int *, double *, double *, double *,
                int *, int *, double *, double *, double *, int *);

void NGCALLF(dspnt3d,DSPNT3D) (int *, double *, double *, double *,
                double *, int *, double *, double *, double *, double *, 
                int *);
void NGCALLF(dspnt2d,DSPNT2D) (int *, double *, double *, double *, int *,
                double *, double *, double *, int *);
void NGCALLF(dsseti,DSSETI) (char *, int *);
void NGCALLF(dsgeti,DSGETI) (char *, int *);
void NGCALLF(dssetrd,DSSETRD) (char *, double *);
void NGCALLF(dsgetrd,DSGETRD) (char *, double *);
void NGCALLF(fdssetc,FDSSETC) (char *, char *, int *);
void NGCALLF(fdsgetc,FDSGETC) (char *, char *, int *);
void NGCALLF(dsgrid3s,DSGRID3S) (int *, float *, float *, float *, float *, 
                int *, int *, int *, float *, float *, float *, float *, 
                int *);
void NGCALLF(dsgrid2s,DSGRID2S) (int *, float *, float *, float *,
               int *, int *, float *, float *, float *, int *);
void NGCALLF(dssetr,DSSETR) (char *, float *);
void NGCALLF(dsgetr,DSGETR) (char *, float *);
void NGCALLF(dspnt3s,DSPNT3S) (int *, float *, float *, float *, float *, 
                int *, float *, float *, float *, float *, int *);
void NGCALLF(dspnt2s,DSPNT2S) (int *, float *, float *, float *, int *,
                float *, float *, float *, int *);
