/*
 *	$Id: c_vvectr.c,v 1.5 2008-07-23 16:17:08 haley Exp $
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

extern void NGCALLF(vvectr,VVECTR)(float*,float*,float*,int*,
                                   int (*vvudmv_)(),float*);

void c_vvectr
#ifdef NeedFuncProto
(
    float *u,
    float *v,
    float *p,
    int *iam,
    int (*vvudmv_)(
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai
    ),
    float *wrk
)
#else
(u,v,p,iam,vvudmv_,wrk)
    float *u;
    float *v;
    float *p;
    int *iam;
    int (*vvudmv_)();
    float *wrk;
#endif
{
    NGCALLF(vvectr,VVECTR)(u,v,p,iam,vvudmv_,wrk);
}
