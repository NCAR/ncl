/*
 *      $Id: c_ctcltr.c,v 1.2 2008-07-23 16:16:44 haley Exp $
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

extern void NGCALLF(ctcltr,CTCLTR)(float*,int*,int*,float*,int*,
                                   float*,int*,int*,int*,int*);

void c_ctcltr
#ifdef NeedFuncProto
(
    float *rpnt,
    int *iedg,
    int *itri,
    float *rwrk,
    int *iwrk,
    float clvl,
    int *ijmp,
    int *irw1,
    int *irw2,
    int *nrwk
)
#else
 (rpnt,iedg,itri,rwrk,iwrk,clvl,ijmp,irw1,irw2,nrwk)
    float *rpnt;
    int *iedg;
    int *itri;
    float *rwrk;
    int *iwrk;
    float clvl;
    int *ijmp;
    int *irw1;
    int *irw2;
    int *nrwk;
#endif
{
    NGCALLF(ctcltr,CTCLTR)(rpnt,iedg,itri,rwrk,iwrk,&clvl,ijmp,irw1,irw2,nrwk);
}
