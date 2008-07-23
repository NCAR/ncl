/*
 *      $Id: c_ctlbam.c,v 1.2 2008-07-23 16:16:44 haley Exp $
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

extern void NGCALLF(ctlbam,CTLBAM)(float*,int*,int*,float*,int*,int*);

void c_ctlbam
#ifdef NeedFuncProto
(
    float *rpnt,
    int *iedg,
    int *itri,
    float *rwrk,
    int *iwrk,
    int *iama
)
#else
(rpnt,iedg,itri,rwrk,iwrk,iama)
    float *rpnt;
    int *iedg;
    int *itri;
    float *rwrk;
    int *iwrk;
    int *iama;
#endif
{
    NGCALLF(ctlbam,CTLBAM)(rpnt,iedg,itri,rwrk,iwrk,iama);
}
