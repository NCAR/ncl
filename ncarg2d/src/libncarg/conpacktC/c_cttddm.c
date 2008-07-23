/*
 *      $Id: c_cttddm.c,v 1.3 2008-07-23 16:16:44 haley Exp $
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

extern void NGCALLF(cttddm,CTTDDM)(float*,int*,int*,float*,int*,int*);

void c_cttddm
#ifdef NeedFuncProto
(
    float *rpnt,
    int *iedg,
    int *itri,
    float *rwrk,
    int *iwrk,
    int idia
)
#else
(rpnt,iedg,itri,rwrk,iwrk,idia)
    float *rpnt;
    int *iedg;
    int *itri;
    float *rwrk;
    int *iwrk;
    int idia;
#endif
{
    NGCALLF(cttddm,CTTDDM)(rpnt,iedg,itri,rwrk,iwrk,&idia);
}
