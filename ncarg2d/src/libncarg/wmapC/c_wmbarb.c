/*
 *	$Id: c_wmbarb.c,v 1.1 1997-04-11 17:45:22 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_wmbarb
#ifdef NeedFuncProto
(
    float x,
    float y,
    float u,
    float v
)
#else
(x,y,u,v)
    float x;
    float y;
    float u;
    float v;
#endif
{
    float x2, y2, u2, v2;
    x2 = x;
    y2 = y;
    u2 = u;
    v2 = v;
    NGCALLF(wmbarb,WMBARB)(&x2,&y2,&u2,&v2);
}
