/*
 *	$Id: c_cpcltr.c,v 1.5 2008-07-23 16:16:42 haley Exp $
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

extern void NGCALLF(cpcltr,CPCLTR)(float*,float*,int*,float*,int*,int*,int*,
                                   int*);

void c_cpcltr
#ifdef NeedFuncProto
(
    float *zdat,
    float *rwrk,
    int *iwrk,
    float clvl,
    int *ijmp,
    int *irw1,
    int *irw2,
    int *nrwk
)
#else
 (zdat,rwrk,iwrk,clvl,ijmp,irw1,irw2,nrwk)
    float *zdat;
    float *rwrk;
    int *iwrk;
    float clvl;
    int *ijmp;
    int *irw1;
    int *irw2;
    int *nrwk;
#endif
{
    NGCALLF(cpcltr,CPCLTR)(zdat,rwrk,iwrk,&clvl,ijmp,irw1,irw2,nrwk);
}
