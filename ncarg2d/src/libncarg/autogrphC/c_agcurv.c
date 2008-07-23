/*
 *	$Id: c_agcurv.c,v 1.5 2008-07-23 16:16:40 haley Exp $
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

extern void NGCALLF(agcurv,AGCURV)(float*,int*,float*,int*,int*,int*);

void c_agcurv
#ifdef NeedFuncProto
(
    float *xvec,
    int iiex,
    float *yvec,
    int iiey,
    int nexy,
    int kdsh
)
#else
(xvec,iiex,yvec,iiey,nexy,kdsh)
    float *xvec;
    int iiex;
    float *yvec;
    int iiey;
    int nexy;
    int kdsh;
#endif
{
    NGCALLF(agcurv,AGCURV)(xvec,&iiex,yvec,&iiey,&nexy,&kdsh);
}
