/*
 *	$Id: c_srface.c,v 1.5 2008-07-23 16:17:03 haley Exp $
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

extern void NGCALLF(srface,SRFACE)(float*,float*,float*,int*,int*,int*,int*,
                                   float*,float*);

void c_srface
#ifdef NeedFuncProto
(
    float *x,
    float *y,
    float *z,
    int *m,
    int mx,
    int nx,
    int ny,
    float s[6],
    float stereo
)
#else
(x,y,z,m,mx,nx,ny,s,stereo)
    float *x;
    float *y;
    float *z;
    int *m;
    int mx;
    int nx;
    int ny;
    float s[6];
    float stereo;
#endif
{
    NGCALLF(srface,SRFACE)(x,y,z,m,&mx,&nx,&ny,s,&stereo);
}
