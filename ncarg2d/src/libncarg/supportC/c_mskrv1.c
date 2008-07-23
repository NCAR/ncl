/*
 *	$Id: c_mskrv1.c,v 1.5 2008-07-23 16:17:04 haley Exp $
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

extern void NGCALLF(mskrv1,MSKRV1)(int*,float*,float*,float*,float*,float*,
                                   float*,float*,float*,float*,int*);

void c_mskrv1 
#ifdef NeedFuncProto
(
    int n,
    float *x,
    float *y,
    float slp1,
    float slpn,
    float *xp,
    float *yp,
    float *temp,
    float *s,
    float sigma,
    int islpsw
)
#else
(n,x,y,slp1,slpn,xp,yp,temp,s,sigma,islpsw)
    int n;
    float *x;
    float *y;
    float slp1;
    float slpn;
    float *xp;
    float *yp;
    float *temp;
    float *s;
    float sigma;
    int islpsw;
#endif
{
    NGCALLF(mskrv1,MSKRV1)(&n,x,y,&slp1,&slpn,xp,yp,temp,s,&sigma,&islpsw);
}
