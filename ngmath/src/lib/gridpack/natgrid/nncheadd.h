/*
 * $Id: nncheadd.h,v 1.6 2008-07-27 04:02:37 haley Exp $
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

#include <ncarg/ngmath.h>

struct asinfod
{  int          crows;
   int          ccols;
   double       **aspect_outd;
   double       **slope_outd;
};
struct asinfod  curasd;

extern double   armind(int, double *);
extern double   armaxd(int, double *);

extern void     Initialized(int, double [], double [], int, int,
                            double [], double []);

int             ReadDatad(int, double *, double *, double *);
double          **MakeGridd(int, int, double *, double *);
