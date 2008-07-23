/*
 *	$Id: c_cprect.c,v 1.5 2008-07-23 16:16:43 haley Exp $
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

extern void NGCALLF(cprect,CPRECT)(float*,int*,int*,int*,float*,int*,int*,
                                   int*);

void c_cprect
#ifdef NeedFuncProto
(
    float *zdat,
    int kzdt,
    int mzdt,
    int nzdt,
    float *rwrk,
    int krwk,
    int *iwrk,
    int kiwk
)
#else
(zdat,kzdt,mzdt,nzdt,rwrk,krwk,iwrk,kiwk)
    float *zdat;
    int kzdt;
    int mzdt;
    int nzdt;
    float *rwrk;
    int krwk;
    int *iwrk;
    int kiwk;
#endif
{
    NGCALLF(cprect,CPRECT)(zdat,&kzdt,&mzdt,&nzdt,rwrk,&krwk,iwrk,&kiwk);
}
