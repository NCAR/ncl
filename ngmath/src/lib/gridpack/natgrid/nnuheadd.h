/*
 * $Id: nnuheadd.h,v 1.11 2008-07-27 04:02:37 haley Exp $
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

extern void   c_nngetsloped(int, int, double *, int *);
extern void   c_nngetaspectd(int, int, double *, int *);
extern void   c_nnpntinitd(int, double *, double *, double *);
extern void   c_nnpntd(double, double, double *);
extern void   c_nnpntendd();
