/*
 *      $Id: c_tdprpt.c,v 1.6 2008-07-23 16:17:06 haley Exp $
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

extern void NGCALLF(tdprpt,TDPRPT)(float*,float*,float*,float*,float*);

void c_tdprpt
#ifdef NeedFuncProto
(
    float  ui3d,
    float  vi3d,
    float  wi3d,
    float *xi2d,
    float *yi2d
)
#else
(ui3d,vi3d,wi3d,xi2d,yi2d)
    float  ui3d;
    float  vi3d;
    float  wi3d;
    float *xi2d;
    float *yi2d;
#endif
{
    NGCALLF(tdprpt,TDPRPT)(&ui3d,&vi3d,&wi3d,xi2d,yi2d);
}
