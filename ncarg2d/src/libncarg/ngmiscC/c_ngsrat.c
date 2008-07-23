/*
 *	$Id: c_ngsrat.c,v 1.5 2008-07-23 16:16:58 haley Exp $
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

extern void NGCALLF(ngsrat,NGSRAT)(int*,int*,float*);

void c_ngsrat
#ifdef NeedFuncProto
(
    int iopt,
    int *iat,
    float *rat
)
#else
(iopt,iat,rat)
    int iopt;
    int *iat;
    float *rat;
#endif
{
    NGCALLF(ngsrat,NGSRAT)(&iopt,iat,rat);
}
