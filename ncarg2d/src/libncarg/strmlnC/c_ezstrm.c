/*
 *	$Id: c_ezstrm.c,v 1.5 2008-07-23 16:17:03 haley Exp $
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

extern void NGCALLF(ezstrm,EZSTRM)(float*,float*,float*,int*,int*);

void c_ezstrm
#ifdef NeedFuncProto
(
    float *u,
    float *v,
    float *work,
    int imax,
    int jmax
)
#else
(u,v,work,imax,jmax)
    float *u;
    float *v;
    float *work;
    int imax;
    int jmax;
#endif
{
    NGCALLF(ezstrm,EZSTRM)(u,v,work,&imax,&jmax);
}
