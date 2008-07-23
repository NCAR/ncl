/*
 *      $Id: c_mputin.c,v 1.2 2008-07-23 16:16:54 haley Exp $
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

#include <ncarg/ncargC.h>

extern void NGCALLF(mputin,MPUTIN)(int*,int*,int*,double*,double*,double*,
				   double*,double*);

void c_mputin
#ifdef NeedFuncProto
(
    int iprj,
    int izon,
    int isph,
    double *para,
    double umin,
    double umax,
    double vmin,
    double vmax
)
#else
(xlat,xlon,ifst,iamp,igrp,idlt,idrt)
    int iprj;
    int izon;
    int isph;
    double *para;
    double umin;
    double umax;
    double vmin;
    double vmax;
#endif
{
    NGCALLF(mputin,MPUTIN)(&iprj,&izon,&isph,para,&umin,&umax,&vmin,&vmax);
}
