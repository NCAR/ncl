/*
 *      $Id: c_tdprpi.c,v 1.5 2008-07-23 16:17:06 haley Exp $
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

extern void NGCALLF(tdprpi,TDPRPI)(float*,float*,float*,float*);

void c_tdprpi
#ifdef NeedFuncProto
(
    float  xi2d,
    float  yi2d,
    float *xipa,
    float *yipa
)
#else
(xi2d,yi2d,xipa,yipa)
    float  xi2d;
    float  yi2d;
    float *xipa;
    float *yipa;
#endif
{
    NGCALLF(tdprpi,TDPRPI)(&xi2d,&yi2d,xipa,yipa);
}
