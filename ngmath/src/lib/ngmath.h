#include <stdio.h>

/*
 *  This file contains the function prototypes for all
 *  user entry points in the ngmath library.
 */


/*
 *  Function prototypes for the dsgrid package.
 */
void     c_dssetc(char *, char *);
void     c_dsgetc(char *, char *);
void     c_dsseti(char *, int);
void     c_dsgeti(char *, int *);
void     c_dsgetr(char *, float *);
void     c_dssetr(char *, float);
void     c_dssetrd(char *, double);
void     c_dsgetrd(char *, double *);
float    *c_dsgrid2s(int, float [], float [], float [],
                     int, int, float [], float [], int *);
double   *c_dsgrid2d(int, double [], double [], double [],
                     int, int, double [], double [], int *);
float    *c_dsgrid3s(int, float [], float [], float [], float [],
                     int, int, int, float [], float [], float [], int *);
double   *c_dsgrid3d(int, double [], double [], double [], double [],
                     int, int, int, double [], double [], double [], int *);
void     c_dspnt3d(int, double [], double [], double [], double [],
                   int, double [], double [], double [], double [], int *);
void     c_dspnt2d(int, double [], double [], double [],
                   int, double [], double [], double [], int *);
void     c_dspnt3s(int, float [], float [], float [], float [],
                   int, float [], float [], float [], float [], int *);
void     c_dspnt2s(int, float [], float [], float [],
                   int, float [], float [], float [], int *);

/*
 *  Function prototypes for the natgrid package.
 */
void    c_nnseti(char *, int);
void    c_nngeti(char *, int *);
void    c_nnsetr(char *, float);
void    c_nngetr(char *, float *);
void    c_nnsetc(char *, char *);
void    c_nngetc(char *, char *);
void    c_nngetslopes(int, int, float *, int *);
void    c_nngetaspects(int, int, float *, int *);
void    c_nnpntinits(int, float [], float [], float []);
void    c_nnpnts(float, float, float *);
void    c_nnpntend();
float   *c_natgrids(int, float [], float [], float [],
                     int, int, float [], float [], int *);

void    c_nnsetrd(char *, double);
void    c_nngetrd(char *, double *);
void    c_nngetsloped(int, int, double *, int *);
void    c_nngetaspectd(int, int, double *, int *);
void    c_nnpntinitd(int, double [], double [], double []);
void    c_nnpntd(double, double, double *);
void    c_nnpntendd();
double  *c_natgridd(int, double [], double [], double [],
                     int, int, double [], double [], int *);
