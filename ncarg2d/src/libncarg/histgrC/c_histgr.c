/*
 *	$Id: c_histgr.c,v 1.5 2008-07-23 16:16:56 haley Exp $
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

extern void NGCALLF(histgr,HISTGR)(float*,int*,int*,int*,float*,int*,
                                   float*,int*);

void c_histgr
#ifdef NeedFuncProto
(
    float *dat1,
    int ndim,
    int npts,
    int iflag,
    float *class_values,
    int nclass,
    float *wrk,
    int nwrk
)
#else
(dat1,ndim,npts,iflag,class_values,nclass,wrk,nwrk)
    float *dat1;
    int ndim;
    int npts;
    int iflag;
    float *class_values;
    int nclass;
    float *wrk;
    int nwrk;
#endif
{
    NGCALLF(histgr,HISTGR)(dat1,&ndim,&npts,&iflag,class_values,&nclass,
                           wrk,&nwrk);
}
