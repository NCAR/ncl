/*
 *	$Id: c_trn32s.c,v 1.1 1997-04-11 17:44:34 haley Exp $
 */
#include <ncarg/ncargC.h>

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
    float x2, y2, z2, xt2, yt2, zt2;
    x2 = x;
    y2 = y;
    z2 = z;
    xt2 = xt;
    yt2 = yt;
    zt2 = zt;
    NGCALLF(trn32s,TRN32S)(&x2,&y2,&z2,&xt2,&yt2,&zt2,&iflag);
}
