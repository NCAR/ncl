/*
 * $Id: nngheadd.h,v 1.6 2008-07-27 04:02:37 haley Exp $
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

void    Initialized(int, double [], double [], int, int,
                    double [], double []);

double  armind(int, double *);
double  armaxd(int, double *);

extern int     ReadDatad();
extern double  **MakeGridd(int, int, double *, double *);

extern void   c_nnsetrd(char *, double);
extern void   c_nngetrd(char *, double *);

extern void   Terminate();

extern int    cull_dtriples(int, double *);
extern int    comp_dtriples(const void *, const void *);
