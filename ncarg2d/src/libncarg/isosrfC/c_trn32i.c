/*
 *	$Id: c_trn32i.c,v 1.5 2008-07-23 16:16:57 haley Exp $
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

extern void NGCALLF(trn32i,TRN32I)(float*,float*,float*,float*,float*,
                                   float*,int*);

void c_trn32i
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
    NGCALLF(trn32i,TRN32I)(&ut,&vt,&wt,&xt,&yt,&zt,&ient);
}
