/*
 *	$Id: c_rgbyiq.c,v 1.1 1997-04-11 17:40:51 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_rgbyiq 
#ifdef NeedFuncProto
(
    float r,
    float g,
    float b,
    float *y,
    float *i,
    float *q
)
#else
 ( r, g, b, y, i, q )
    float r;
    float g;
    float b;
    float *y;
    float *i;
    float *q;
#endif
{
    float r2, g2, b2;
    r2 = r;
    g2 = g;
    b2 = b;
    NGCALLF(rgbyiq,RGBYIQ)( &r2, &g2, &b2, y, i, q );
}
