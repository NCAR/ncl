/*
 *	$Id: c_idbvip.c,v 1.4 2008-07-23 16:17:04 haley Exp $
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

extern void NGCALLF(idbvip,IDBVIP)(int*,int*,float*,float*,float*,int*,
                                   float*,float*,float*,int*,float*);

void c_idbvip
#ifdef NeedFuncProto
(
    int md,
    int ndp,
    float *xd,
    float *yd,
    float *zd,
    int nip,
    float *xi,
    float *yi,
    float *zi,
    int *iwk,
    float *wk
)
#else
(md,ndp,xd,yd,zd,nip,xi,yi,zi,iwk,wk)
    int md;
    int ndp;
    float *xd;
    float *yd;
    float *zd;
    int nip;
    float *xi;
    float *yi;
    float *zi;
    int *iwk;
    float *wk;
#endif
{
    NGCALLF(idbvip,IDBVIP)(&md,&ndp,xd,yd,zd,&nip,xi,yi,zi,iwk,wk);
}
