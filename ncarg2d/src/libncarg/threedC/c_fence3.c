/*
 *	$Id: c_fence3.c,v 1.1 1997-04-11 17:45:08 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_fence3
#ifdef NeedFuncProto
(
    float *u,
    float *v,
    float *w,
    int n,
    int ior,
    float bot
)
#else
(u,v,w,n,ior,bot)
    float *u;
    float *v;
    float *w;
    int n;
    int ior;
    float bot;
#endif
{
    float bot2;

    bot2 = bot;
    NGCALLF(fence3,FENCE3)(u,v,w,&n,&ior,&bot2);
}
