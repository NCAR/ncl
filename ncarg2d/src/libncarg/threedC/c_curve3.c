/*
 *	$Id: c_curve3.c,v 1.1 1997-04-11 17:45:08 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_curve3 
#ifdef NeedFuncProto
(
    float *u,
    float *v,
    float *w,
    int n
)
#else
 (u,v,w,n)
    float *u;
    float *v;
    float *w;
    int n;
#endif
{
    NGCALLF(curve3,CURVE3)(u,v,w,&n);
}
