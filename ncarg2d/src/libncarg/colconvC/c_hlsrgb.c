/*
 *	$Id: c_hlsrgb.c,v 1.1 1997-04-11 17:40:49 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_hlsrgb
#ifdef NeedFuncProto
(
    float h,
    float l,
    float s,
    float *r,
    float *g,
    float *b
)
#else
(h,l,s,r,g,b)
    float h;
    float l;
    float s;
    float *r;
    float *g;
    float *b;
#endif
{
    float h2, l2, s2;
    h2 = h;
    l2 = l;
    s2 = s;
    NGCALLF(hlsrgb,HLSRGB)( &h2, &l2, &s2, r, g, b );
}   
