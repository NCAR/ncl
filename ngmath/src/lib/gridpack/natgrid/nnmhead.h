/*
 * $Id: nnmhead.h,v 1.13 2008-07-27 04:02:37 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define SQ(x)   (x) * (x)
#define RANSEED        367367
#define BIGNUM         1E37
#define EPSILON        0.00001
#define EQ             ==
#define NE             !=
#define AND            &&
#define OR             ||

double  **points, **joints, wbit, 
        horilap = -1., vertlap = -1., bI = 1.5, bJ = 7.0, nuldat = 0.0,
        xstart, ystart, xend, yend,
        maxhoriz, aaa, bbb, ccc, det,
        work3[3][3], xx, sumx, sumy, sumz,
        sumx2, sumy2, sumxy, sumxz, sumyz,
        asum, nn_pi, piby2, piby32, rad2deg,
        bigtri[3][3], horilap_save, vertlap_save;

double  magx = 1, magy = 1, magz = 1, magx_orig, magy_orig, magz_orig, 
        maxxy[2][3], magx_auto, magy_auto, magz_auto, *wts;

int     igrad = 0, non_neg = 0, densi, sdip = 0, rads = 0, southhemi = 0,
        extrap = 1, adf = 0, nndup = 1, maxmsg = 10;

int     datcnt, datcnt3, numtri, imag, numnei, iscale,
        ext, *jndx, neicnt, optim = 1, goodflag, updir = 1,
        scor[3][2] = {{1,2}, {2,0}, {0,1}}, auto_scale = 1,
        single_point = 0, first_single = 1, asflag = 1,
        error_status = 0, *nbrs, jwts = 0, num_wts;

char    tri_file[256] = {"nnalg.dat"}, error_file[256] = {"stderr"},
        emsg[256];

extern void   Gradient();
extern void   ErrorHnd(int, char *, FILE *, char *);
extern void   CircOut();

extern void   c_nnsetc(char *, char *);
extern void   c_nngetc(char *, char *);
extern void   c_nnseti(char *, int);
extern void   c_nngeti(char *, int *);

extern void     Terminate();
extern void     ErrorHnd(int, char *, FILE *, char *);

void            FindNeigh();
void            TriNeigh();
void            Gradient();
void            FindProp();
double          GridSurface();
double          Meld();
void            TooSteep();
void            TooShallow();
void            TooNarrow();
struct datum    *IMakeDatum();
struct simp     *IMakeSimp();
struct temp     *IMakeTemp();
struct neig     *IMakeNeig();
int             *IntVect();
void            FreeVecti();
double          *DoubleVect();
void            FreeVectd();
int             **IntMatrix();
void            FreeMatrixi();
float           **FloatMatrix();
void            FreeMatrixf();
double          **DoubleMatrix();
void            FreeMatrixd();
