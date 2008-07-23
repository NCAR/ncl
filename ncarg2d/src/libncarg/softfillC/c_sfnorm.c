/*
 *	$Id: c_sfnorm.c,v 1.5 2008-07-23 16:17:00 haley Exp $
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

extern void NGCALLF(sfnorm,SFNORM)(float*,float*,int*,float*,int*,int*,int*);

void c_sfnorm
#ifdef NeedFuncProto
(
    float *xra,
    float *yra,
    int nra,
    float *dst,
    int nst,
    int *ind,
    int nnd
)
#else
(xra,yra,nra,dst,nst,ind,nnd)
    float *xra;
    float *yra;
    int nra;
    float *dst;
    int nst;
    int *ind;
    int nnd;
#endif
{
    NGCALLF(sfnorm,SFNORM)(xra,yra,&nra,dst,&nst,ind,&nnd);
}    
