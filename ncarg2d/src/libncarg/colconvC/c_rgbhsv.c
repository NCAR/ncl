/*
 *	$Id: c_rgbhsv.c,v 1.1 1997-04-11 17:40:50 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_rgbhsv
#ifdef NeedFuncProto
(
    float r,
    float g,
    float b,
    float *h,
    float *s,
    float *v
)
#else
( r, g, b, h, s, v )
    float r;
    float g;
    float b;
    float *h;
    float *s;
    float *v;
#endif
{
    float r2, g2, b2;
    r2 = r;
    g2 = g;
    b2 = b;
    NGCALLF(rgbhsv,RGBHSV)( &r2, &g2, &b2, h, s, v );
}   
