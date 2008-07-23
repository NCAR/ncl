/*
 *	$Id: c_ezvec.c,v 1.5 2008-07-23 16:17:08 haley Exp $
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

extern void NGCALLF(ezvec,EZVEC)(float*,float*,int*,int*);

void c_ezvec
#ifdef NeedFuncProto
(
    float *u,
    float *v,
    int m,
    int n
)
#else
(u,v,m,n)
    float *u;
    float *v;
    int m;
    int n;
#endif
{
    NGCALLF(ezvec,EZVEC)(u,v,&m,&n);
}
