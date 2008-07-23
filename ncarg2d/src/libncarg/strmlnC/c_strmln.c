/*
 *	$Id: c_strmln.c,v 1.5 2008-07-23 16:17:04 haley Exp $
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

extern void NGCALLF(strmln,STRMLN)(float*,float*,float*,int*,int*,int*,int*,
                                   int*);

void c_strmln
#ifdef NeedFuncProto
(
    float *u,
    float *v,
    float *work,
    int imax,
    int iptsx,
    int jptsy,
    int nset,
    int *ier
)
#else
(u,v,work,imax,iptsx,jptsy,nset,ier)
    float *u;
    float *v;
    float *work;
    int imax;
    int iptsx;
    int jptsy;
    int nset;
    int *ier;
#endif
{
    NGCALLF(strmln,STRMLN)(u,v,work,&imax,&iptsx,&jptsy,&nset,ier);
}
