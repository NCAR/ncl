/*
 * $Id: nnghead.h,v 1.10 2008-07-27 04:02:37 haley Exp $
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
#include <time.h>

#define RANSEED        367367
#define BIGNUM         1E37
#define EPSILON        0.00001
#define EQ             ==
#define NE             !=
#define AND            &&
#define OR             ||

extern  double  **points, **joints, wbit,
                horilap, vertlap, bI, bJ, nuldat,
                xstart, ystart, xend, yend,
                maxhoriz, aaa, bbb, ccc, det,
                work3[3][3], xx, sumx, sumy, sumz,
                sumx2, sumy2, sumxy, sumxz, sumyz,
                asum, nn_pi, piby2, piby32, rad2deg,
                bigtri[3][3], horilap_save, vertlap_save;

extern  double  magx, magy, magz, magx_orig, magy_orig, magz_orig,
                maxxy[2][3], magx_auto, magy_auto, magz_auto, *wts;

extern  int     igrad, non_neg, densi, sdip, rads, southhemi,
                extrap, adf, nndup, maxmsg;

extern  int     datcnt, datcnt3, numtri, imag, numnei, iscale,
                ext, *jndx, neicnt, optim, goodflag, updir,
                scor[3][2], auto_scale,
                single_point, first_single, asflag,
                error_status, *nbrs, jwts, num_wts;

extern  char    tri_file[256], error_file[256], emsg[256];

extern void   Gradient();
extern void   ErrorHnd(int, char *, FILE *, char *);
extern void   CircOut();

extern void   c_nnsetc(char *, char *);
extern void   c_nngetc(char *, char *);
extern void   c_nnseti(char *, int);
extern void   c_nngeti(char *, int *);
