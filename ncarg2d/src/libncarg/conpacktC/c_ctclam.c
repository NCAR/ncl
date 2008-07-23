/*
 *      $Id: c_ctclam.c,v 1.2 2008-07-23 16:16:43 haley Exp $
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

extern void NGCALLF(ctclam,CTCLAM)(float*,int*,int*,float*,int*,int*);

void c_ctclam
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
    NGCALLF(ctclam,CTCLAM)(rpnt,iedg,itri,rwrk,iwrk,iama);
}
