/*
 *      $Id: c_tdmtri.c,v 1.5 2008-07-23 16:17:06 haley Exp $
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

extern void NGCALLF(tdmtri,TDMTRI)(int*,float*,float*,float*,float*,float*,
                                   int*,int*,int*,float*,float*,float*,
                                   float*,float*,float*);

void c_tdmtri
#ifdef NeedFuncProto
(
    int    imrk,
    float  umrk,
    float  vmrk,
    float  wmrk,
    float  smrk,
    float *rtri,
    int    mtri,
    int   *ntri,
    int    irst,
    float  umin,
    float  vmin,
    float  wmin,
    float  umax,
    float  vmax,
    float  wmax
)
#else
(imrk,umrk,vmrk,wmrk,smrk,rtri,mtri,ntri,irst,umin,vmin,wmin,umax,vmax,wmax)
    int    imrk;
    float  umrk;
    float  vmrk;
    float  wmrk;
    float  smrk;
    float *rtri;
    int    mtri;
    int   *ntri;
    int    irst;
    float  umin;
    float  vmin;
    float  wmin;
    float  umax;
    float  vmax;
    float  wmax;
#endif
{
    NGCALLF(tdmtri,TDMTRI)(&imrk,&umrk,&vmrk,&wmrk,&smrk,rtri,&mtri,
                   ntri,&irst,&umin,&vmin,&wmin,&umax,&vmax,&wmax);
}
