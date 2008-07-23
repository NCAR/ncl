/*
 *      $Id: c_ctcldr.c,v 1.2 2008-07-23 16:16:44 haley Exp $
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

extern void NGCALLF(ctcldr,CTCLDR)(float*,int*,int*,float*,int*);

void c_ctcldr
#ifdef NeedFuncProto
(
    float *rpnt,
    int *iedg,
    int *itri,
    float *rwrk,
    int *iwrk
)
#else
(rpnt,iedg,itri,rwrk,iwrk)
    float *rpnt;
    int *iedg;
    int *itri;
    float *rwrk;
    int *iwrk;
#endif
{
    NGCALLF(ctcldr,CTCLDR)(rpnt,iedg,itri,rwrk,iwrk);
}
