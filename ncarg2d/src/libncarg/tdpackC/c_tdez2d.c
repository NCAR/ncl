/*
 *      $Id: c_tdez2d.c,v 1.6 2008-07-23 16:17:05 haley Exp $
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

extern void NGCALLF(tdez2d,TDEZ2D)(int*,int*,float*,float*,float*,float*,
                                   float*,float*,int*);

void c_tdez2d
#ifdef NeedFuncProto
(
    int nx,
    int ny,
    float *x,
    float *y,
    float *z,
    float rmult,
    float theta,
    float phi,
    int ist
)
#else
 (nx,ny,x,y,z,rmult,theta,phi,ist)
    int nx,
    int ny,
    float *x,
    float *y,
    float *z,
    float rmult,
    float theta,
    float phi,
    int ist
#endif
{
    NGCALLF(tdez2d,TDEZ2D)(&nx,&ny,x,y,z,&rmult,&theta,&phi,&ist);
}
