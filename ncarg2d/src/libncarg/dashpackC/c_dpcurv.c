/*
 *	$Id: c_dpcurv.c,v 1.5 2008-07-23 16:16:45 haley Exp $
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

extern void NGCALLF(dpcurv,DPCURV)(float*,float*,int*);

void c_dpcurv
#ifdef NeedFuncProto
(
    float *xcpu,
    float *ycpu,
    int npts
)
#else
(xcpu,ycpu,npts)
    float *xcpu;
    float *ycpu;
    int npts;
#endif
{
    NGCALLF(dpcurv,DPCURV)(xcpu,ycpu,&npts);
}
