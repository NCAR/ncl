/*
 *      $Id: c_mdpitm.c,v 1.2 2008-07-23 16:16:50 haley Exp $
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

extern void NGCALLF(mdpitm,MDPITM)(double*,double*,int*,int*,float*,float*,
                                   int*,int*,int*,int*,int (*lpr_)());

void c_mdpitm
#ifdef NeedFuncProto
(
    double xlat,
    double xlon,
    int ifst,
    int *iam,
    float *xcs,
    float *ycs,
    int mcs,
    int *iai,
    int *iag,
    int mai,
    int (*lpr_)(
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai
            )
)
#else
 (xlat,xlon,ifst,iam,xcs,ycs,mcs,iai,iag,mai,lpr_)
    double xlat;
    double xlon;
    int ifst;
    int *iam;
    float *xcs;
    float *ycs;
    int mcs;
    int *iai;
    int *iag;
    int mai;
    int (*lpr_)();
#endif
{
    NGCALLF(mdpitm,MDPITM)(&xlat,&xlon,&ifst,iam,xcs,ycs,&mcs,iai,iag,&mai,
                           lpr_);
}
