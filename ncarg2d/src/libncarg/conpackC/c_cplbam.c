/*
 *	$Id: c_cplbam.c,v 1.5 2008-07-23 16:16:43 haley Exp $
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

extern void NGCALLF(cplbam,CPLBAM)(float*,float*,int*,int*);

void c_cplbam
#ifdef NeedFuncProto
(
    float *zdat,
    float *rwrk,
    int *iwrk,
    int *iama
)
#else
(zdat,rwrk,iwrk,iama)
    float *zdat;
    float *rwrk;
    int *iwrk;
    int *iama;
#endif
{
    NGCALLF(cplbam,CPLBAM)(zdat,rwrk,iwrk,iama);
}
