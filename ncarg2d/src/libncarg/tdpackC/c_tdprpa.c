/*
 *      $Id: c_tdprpa.c,v 1.5 2008-07-23 16:17:06 haley Exp $
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

extern void NGCALLF(tdprpa,TDPRPA)(float*,float*,float*,float*);

void c_tdprpa
#ifdef NeedFuncProto
(
    float  xipa,
    float  yipa,
    float *xi2d,
    float *yi2d
)
#else
(xipa,yipa,xi2d,yi2d)
    float  xipa;
    float  yipa;
    float *xi2d;
    float *yi2d;
#endif
{
    NGCALLF(tdprpa,TDPRPA)(&xipa,&yipa,xi2d,yi2d);
}
