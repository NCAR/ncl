/*
 *	$Id: c_wmdrft.c,v 1.1 1997-04-11 17:45:23 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_wmdrft
#ifdef NeedFuncProto
(
    int n,
    float *x,
    float *y
)
#else
(n,x,y)
    int n;
    float *x;
    float *y;
#endif
{
    NGCALLF(wmdrft,WMDRFT)(&n,x,y);
}
