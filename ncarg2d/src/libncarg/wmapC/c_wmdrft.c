/*
 *	$Id: c_wmdrft.c,v 1.5 2008-07-23 16:17:08 haley Exp $
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

extern void NGCALLF(wmdrft,WMDRFT)(int*,float*,float*);

void c_wmdrft
#ifdef NeedFuncProto
(
    int n,
    float *x,
    float *y
)
#else
(n,x,y)
    int n;
    float *x;
    float *y;
#endif
{
    NGCALLF(wmdrft,WMDRFT)(&n,x,y);
}
