/*
 *	$Id: c_cpcldm.c,v 1.5 2008-07-23 16:16:42 haley Exp $
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

extern void NGCALLF(cpcldm,CPCLDM)(float*,float*,int*,int*,int (*rtpl_)());

void c_cpcldm
#ifdef NeedFuncProto
(
    float *zdat,
    float *rwrk,
    int *iwrk,
    int *iama,
    int (*rtpl_)(
        float *xcra,
        float *ycra,
        int *ncra,
        int *iaia,
        int *igia,
        int *nagi
              )
)
#else
(zdat,rwrk,iwrk,iama,rtpl_)
    float *zdat;
    float *rwrk;
    int *iwrk;
    int *iama;
    int (*rtpl_)();
#endif
{
    NGCALLF(cpcldm,CPCLDM)(zdat,rwrk,iwrk,iama,rtpl_);
}
