/*
 *	$Id: c_wmrgwt.c,v 1.5 2008-07-23 16:17:09 haley Exp $
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

extern void NGCALLF(wmrgwt,WMRGWT)(int*,float*,float*,int*,int*);

void c_wmrgwt
#ifdef NeedFuncProto
(
    int n,
    float *x,
    float *y,
    int ifnt,
    int nasc
)
#else
(n,x,y,ifnt,nasc)
    int n;
    float *x;
    float *y;
    int ifnt;
    int nasc;
#endif
{
    NGCALLF(wmrgwt,WMRGWT)(&n,x,y,&ifnt,&nasc);
}
