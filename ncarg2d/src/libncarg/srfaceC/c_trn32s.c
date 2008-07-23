/*
 *	$Id: c_trn32s.c,v 1.5 2008-07-23 16:17:03 haley Exp $
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

extern void NGCALLF(trn32s,TRN32S)(float*,float*,float*,float*,float*,
                                   float*,int*);

void c_trn32s
#ifdef NeedFuncProto
(
    float x,
    float y,
    float z,
    float xt,
    float yt,
    float zt,
    int iflag
)
#else
(x,y,z,xt,yt,zt,iflag)
    float x;
    float y;
    float z;
    float xt;
    float yt;
    float zt;
    int iflag;
#endif
{
    NGCALLF(trn32s,TRN32S)(&x,&y,&z,&xt,&yt,&zt,&iflag);
}
