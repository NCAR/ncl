/*
 *	$Id: c_curved.c,v 1.5 2008-07-23 16:16:45 haley Exp $
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

extern void NGCALLF(curved,CURVED)(float*,float*,int*);

void c_curved
#ifdef NeedFuncProto
(
    float *x,
    float *y,
    int n
)
#else
(x,y,n)
    float *x;
    float *y;
    int n;
#endif
{
    NGCALLF(curved,CURVED)(x,y,&n);
}
