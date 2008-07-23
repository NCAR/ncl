/*
 *      $Id: c_ctcldm.c,v 1.3 2008-07-23 16:16:44 haley Exp $
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

extern void NGCALLF(ctcldm,CTCLDM)(float*,int*,int*,
                                   float*,int*,int*,int (*rtpl_)());

void c_ctcldm
#ifdef NeedFuncProto
(
    float *rpnt,
    int *iedg,
    int *itri,
    float *rwrk,
    int *iwrk,
    int *iama,
    int (*rtpl_)(
        float *xcra,
        float *ycra,
        int *ncra,
        int *iaia,
        int *igia,
        int *nagi)
)
#else
(rpnt,iedg,itri,rwrk,iwrk,iama,rtpl_)
    float *rpnt;
    int *iedg;
    int *itri;
    float *rwrk;
    int *iwrk;
    int *iama;
    int (*rtpl_)();
#endif
{
    NGCALLF(ctcldm,CTCLDM)(rpnt,iedg,itri,rwrk,iwrk,iama,rtpl_);
}
