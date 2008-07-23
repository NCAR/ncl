/*
 *      $Id: c_tdcurv.c,v 1.2 2008-07-23 16:17:05 haley Exp $
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

extern void NGCALLF(tdcurv,TDCURV)(float*,float*,float*,int*,int*,
				   float*,float*);

void c_tdcurv
#ifdef NeedFuncProto
(
    float *ucrv,
    float *vcrv,
    float *wcrv,
    int    ncrv,
    int    iarh,
    float  arhl,
    float  arhw
)
#else
(ucrv,vcrv,wcrv,ncrv,iarh,arhl,arhw)
    float *ucrv;
    float *vcrv;
    float *wcrv;
    int    ncrv;
    int    iarh;
    float  arhl;
    float  arhw;
#endif
{
    NGCALLF(tdcurv,TDCURV)(ucrv,vcrv,wcrv,&ncrv,&iarh,&arhl,&arhw);
}
