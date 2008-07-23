/*
 *	$Id: c_msntvl.c,v 1.4 2008-07-23 16:17:05 haley Exp $
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

extern int NGCALLF(msntvl,MSNTVL)(float*,float*,int*);

int c_msntvl
#ifdef NeedFuncProto
(
    int n,
    float t, 
    float *x
)
#else
(n,t,x)
    int n;
    float t; 
    float *x;
#endif
{
    return(NGCALLF(msntvl,MSNTVL)(&t,x,&n));
}
