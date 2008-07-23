/*
 *	$Id: c_arscam.c,v 1.5 2008-07-23 16:16:40 haley Exp $
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

extern void NGCALLF(arscam,ARSCAM)(int*,float*,float*,int*,int*,int*,
                                   int*,int (*cpcolr_)());


void c_arscam
#ifdef NeedFuncProto
(
    int *iam,
    float *xcs,
    float *ycs,
    int mcs,
    int *iai,
    int *iag,
    int mai,
    int (*cpcolr_)(
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai
               )
)
#else
(iam,xcs,ycs,mcs,iai,iag,mai,cpcolr_)
    int *iam;
    float *xcs;
    float *ycs;
    int mcs;
    int *iai;
    int *iag;
    int mai;
    int (*cpcolr_)();
#endif
{
    NGCALLF(arscam,ARSCAM)(iam,xcs,ycs,&mcs,iai,iag,&mai,cpcolr_);
}
