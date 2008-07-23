/*
 *	$Id: c_getset.c,v 1.5 2008-07-23 16:17:01 haley Exp $
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

extern void NGCALLF(getset,GETSET)(float*,float*,float*,float*,float*,
                                   float*,float*,float*,int*);

void c_getset
#ifdef NeedFuncProto
(
    float *vl,
    float *vr,
    float *vb,
    float *vt,
    float *wl,
    float *wr,
    float *wb,
    float *wt,
    int *lf
)
#else
(vl,vr,vb,vt,wl,wr,wb,wt,lf)
    float *vl;
    float *vr;
    float *vb;
    float *vt;
    float *wl;
    float *wr;
    float *wb;
    float *wt;
    int *lf;
#endif
{
    NGCALLF(getset,GETSET)(vl,vr,vb,vt,wl,wr,wb,wt,lf);
}
