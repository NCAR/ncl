/*
 *  Copyright (C) 1991-1996
 *          by
 *    David F. Watson
 */

#include <stdio.h>
#include <stdlib.h>    /* added 4 Nov. 94 */
#include <math.h>
#include <malloc.h>
#include <time.h>

#define RANSEED        367367
#define BIGNUM         1E37
#define EPSILON        0.00001
#define EQ             ==
#define NE             !=
#define AND            &&
#define OR             ||

double  horilap = -1., vertlap = -1., bI = 1.5, bJ = 7.0, nuldat = 0.0,
        xstart, ystart, xend, yend;
extern  double magx, magy, magz, magx_orig, magy_orig, magz_orig, maxxy[2][3],
        magx_auto, magy_auto, magz_auto;
int     igrad = 0, non_neg = 0, densi, sdip = 0, rads = 0, southhemi = 0,
        extrap = 1, adf = 0, dup = 1;
extern  int datcnt, imag, optim, error_status, iscale, single_point, 
        first_single, asflag;

FILE    *fopen(), *filee = stderr;

void                   Initialize(int, float [], float [], int, int, 
                                  float [], float []);
void                   Terminate();
void                   GetOptions();
void                   InitScrn();
void                   InterpStr();
void                   ShowMenu();
void                   ShowState();
void                   WriteInit();
void                   MakeGnufl();
void                   MakeToGif();
void                   Spacer();
int                    GetSize();
double                 armin(int, float *);
double                 armax(int, float *);

extern int             ReadData();
extern void            Gradient();
extern float           **MakeGrid(int, int, float *, float *);
extern void            GifMaker();
extern void            ErrorHnd(int, char *, FILE *, char *);
extern void            CircOut();

extern void   c_nnsetc(char *, char *);
extern void   c_nngetc(char *, char *);
extern void   c_nnseti(char *, int);
extern void   c_nngeti(char *, int *);
extern void   c_nnsetr(char *, float);
extern void   c_nngetr(char *, float *);
