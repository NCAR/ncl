/*
 *      $Id: c_tdgrds.c,v 1.5 2008-07-23 16:17:06 haley Exp $
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

extern void NGCALLF(tdgrds,TDGRDS)(float*,float*,float*,float*,float*,float*,
                                   float*,float*,float*,int*,int*);

void c_tdgrds
#ifdef NeedFuncProto
(
    float umin,
    float vmin,
    float wmin,
    float umax,
    float vmax,
    float wmax,
    float ustp,
    float vstp,
    float wstp,
    int   igrt,
    int   ihid
)
#else
(umin,vmin,wmin,umax,vmax,wmax,ustp,vstp,wstp,igrt,ihid)
    float umin;
    float vmin;
    float wmin;
    float umax;
    float vmax;
    float wmax;
    float ustp;
    float vstp;
    float wstp;
    int   igrt;
    int   ihid;
#endif
{
    NGCALLF(tdgrds,TDGRDS)(&umin,&vmin,&wmin,&umax,&vmax,&wmax,
                                 &ustp,&vstp,&wstp,&igrt,&ihid);
}
