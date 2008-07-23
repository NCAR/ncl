/*
 *	$Id: c_set.c,v 1.5 2008-07-23 16:17:02 haley Exp $
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

extern void NGCALLF(set,SET)(float*,float*,float*,float*,float*,float*,
                             float*,float*,int*);

void c_set
#ifdef NeedFuncProto
(
    float vl,
    float vr,
    float vb,
    float vt,
    float wl,
    float wr,
    float wb,
    float wt,
    int lf
)
#else
(vl,vr,vb,vt,wl,wr,wb,wt,lf)
    float vl;
    float vr;
    float vb;
    float vt;
    float wl;
    float wr;
    float wb;
    float wt;
    int lf;
#endif
{
    NGCALLF(set,SET)(&vl,&vr,&vb,&vt,&wl,&wr,&wb,&wt,&lf);
}
