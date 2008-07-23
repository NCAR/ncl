/*
 *	$Id: c_msshch.c,v 1.5 2008-07-23 16:17:05 haley Exp $
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

extern void NGCALLF(msshch,MSSHCH)(float*,float*,float*,int*);


void c_msshch 
#ifdef NeedFuncProto
(
    float *sinhm,
    float *coshm,
    float x,
    int isw
)
#else
(sinhm,coshm,x,isw)
    float *sinhm;
    float *coshm;
    float x;
    int isw;
#endif
{
    NGCALLF(msshch,MSSHCH)(sinhm,coshm,&x,&isw);
}
