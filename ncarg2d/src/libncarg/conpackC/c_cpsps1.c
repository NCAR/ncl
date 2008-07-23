/*
 *	$Id: c_cpsps1.c,v 1.5 2008-07-23 16:16:43 haley Exp $
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

extern void NGCALLF(cpsps1,CPSPS1)(float*,int*,int*,int*,float*,int*,int*,
                                   int*,float*,int*);

void c_cpsps1
#ifdef NeedFuncProto
(
    float *zsps,
    int ksps,
    int msps,
    int nsps,
    float *rwrk,
    int krwk,
    int *iwrk,
    int kiwk,
    float *zdat,
    int kzdt
)
#else
(zsps,ksps,msps,nsps,rwrk,krwk,iwrk,kiwk,zdat,kzdt)
    float *zsps;
    int ksps;
    int msps;
    int nsps;
    float *rwrk;
    int krwk;
    int *iwrk;
    int kiwk;
    float *zdat;
    int kzdt;
#endif
{
    NGCALLF(cpsps1,CPSPS1)(zsps,&ksps,&msps,&nsps,rwrk,&krwk,iwrk,&kiwk,zdat,&kzdt);
}
