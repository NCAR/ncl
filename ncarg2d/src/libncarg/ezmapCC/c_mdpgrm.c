/*
 *      $Id: c_mdpgrm.c,v 1.2 2008-07-23 16:16:50 haley Exp $
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

extern void NGCALLF(mdpgrm,MDPGRM)(int*,float*,float*,int*,int*,int*,int*,
                                   int (*lpr_)());

void c_mdpgrm
#ifdef NeedFuncProto
(
    int *iam,
    float *xcs,
    float *ycs,
    int mcs,
    int *iai,
    int *iag,
    int mai,
    int (*lpr_)(
        float *xcra,
        float *ycra,
        int *ncra,
        int *iaia,
        int *igia,
        int *nagi
            )
)
#else
(iam,xcs,ycs,mcs,iai,iag,mai,lpr_)
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
    NGCALLF(mdpgrm,MDPGRM)(iam,xcs,ycs,&mcs,iai,iag,&mai,lpr_);
}
