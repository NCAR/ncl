/*
 *	$Id: c_stinit.c,v 1.1 1997-04-11 17:44:49 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_stinit
#ifdef NeedFuncProto
(
    float *u,
    int lu,
    float *v,
    int lv,
    float *p,
    int lp,
    int m,
    int n,
    float *wrk,
    int lw
)
#else
(u,lu,v,lv,p,lp,m,n,wrk,lw)
    float *u;
    int lu;
    float *v;
    int lv;
    float *p;
    int lp;
    int m;
    int n;
    float *wrk;
    int lw;
#endif
{
    NGCALLF(stinit,STINIT)(u,&lu,v,&lv,p,&lp,&m,&n,wrk,&lw);
}
