/*
 *	$Id: c_yiqrgb.c,v 1.1 1997-04-11 17:40:51 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_yiqrgb
#ifdef NeedFuncProto
(
    float y,
    float i,
    float q,
    float *r,
    float *g,
    float *b
)
#else
 ( y, i, q, r, g, b )
    float y;
    float i;
    float q;
    float *r;
    float *g;
    float *b;
#endif
{
    float y2, i2, q2;
    y2 = y;
    i2 = i;
    q2 = q;
    NGCALLF(yiqrgb,YIQRGB)( &y2, &i2, &q2, r, g, b );
}
