/*
 *	$Id: c_wmrgwt.c,v 1.1 1997-04-11 17:45:29 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_wmrgwt
#ifdef NeedFuncProto
(
    int n,
    float *x,
    float *y,
    int ifnt,
    int nasc
)
#else
(n,x,y,ifnt,nasc)
    int n;
    float *x;
    float *y;
    int ifnt;
    int nasc;
#endif
{
    NGCALLF(wmrgwt,WMRGWT)(&n,x,y,&ifnt,&nasc);
}
