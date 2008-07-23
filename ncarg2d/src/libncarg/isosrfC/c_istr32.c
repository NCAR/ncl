/*
 *	$Id: c_istr32.c,v 1.5 2008-07-23 16:16:56 haley Exp $
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

extern void NGCALLF(istr32,ISTR32)(float*,float*,float*,float*,float*,
                                   float*,int*);

void c_istr32
#ifdef NeedFuncProto
(
    float ut,
    float vt,
    float wt,
    float xt,
    float yt,
    float zt,
    int ient
)
#else
(ut,vt,wt,xt,yt,zt,ient)
    float ut;
    float vt;
    float wt;
    float xt;
    float yt;
    float zt;
    int ient;
#endif
{
    NGCALLF(istr32,ISTR32)(&ut,&vt,&wt,&xt,&yt,&zt,&ient);
}
