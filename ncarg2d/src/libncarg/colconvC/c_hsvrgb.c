/*
 *	$Id: c_hsvrgb.c,v 1.1 1997-04-11 17:40:49 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_hsvrgb
#ifdef NeedFuncProto
(
    float h,
    float s,
    float v,
    float *r,
    float *g,
    float *b
)
#else
(h,s,v,r,g,b)
    float h;
    float s;
    float v;
    float *r;
    float *g;
    float *b;
#endif
{
    float h1,s1,v1;
    h1 = h;
    s1 = s;
    v1 = v;
    NGCALLF(hsvrgb,HSVRGB)(&h1,&s1,&v1,r,g,b);
}
