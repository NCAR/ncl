/*
 *	$Id: c_rgbhls.c,v 1.1 1997-04-11 17:40:50 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_rgbhls
#ifdef NeedFuncProto
(
    float r,
    float g,
    float b,
    float *h,
    float *l,
    float *s
)
#else
( r, g, b, h, l, s )
    float r;
    float g;
    float b;
    float *h;
    float *l;
    float *s;
#endif
{
    float r2, g2, b2;
    r2 = r;
    g2 = g;
    b2 = b;
    NGCALLF(rgbhls,RGBHLS)( &r2, &g2, &b2, h, l, s );
}
