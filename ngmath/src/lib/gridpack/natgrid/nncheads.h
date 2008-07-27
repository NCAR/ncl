/*
 * $Id: nncheads.h,v 1.7 2008-07-27 04:02:37 haley Exp $
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
extern double   armin(int, float *);
extern double   armax(int, float *);

extern void     Initialize(int, float [], float [], int, int, 
                           float [], float []);
int             ReadData(int, float *, float *, float *);
float           **MakeGrid(int, int, float *, float *);
