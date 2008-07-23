/*
 *	$Id: c_cpcnrc.c,v 1.5 2008-07-23 16:16:42 haley Exp $
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

extern void NGCALLF(cpcnrc,CPCNRC)(float*,int*,int*,int*,float*,float*,
                                   float*,int*,int*,int*);

void c_cpcnrc
#ifdef NeedFuncProto
(
    float *zdat,
    int kzdt,
    int mzdt,
    int nzdt,
    float flow,
    float fhgh,
    float finc,
    int kset,
    int nhgh,
    int ndsh
)
#else
(zdat,kzdt,mzdt,nzdt,flow,fhgh,finc,kset,nhgh,ndsh)
    float *zdat;
    int kzdt;
    int mzdt;
    int nzdt;
    float flow;
    float fhgh;
    float finc;
    int kset;
    int nhgh;
    int ndsh;
#endif
{
    NGCALLF(cpcnrc,CPCNRC)(zdat,&kzdt,&mzdt,&nzdt,&flow,&fhgh,&finc,&kset,
                           &nhgh,&ndsh);
}
