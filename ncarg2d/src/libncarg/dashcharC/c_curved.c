/*
 *	$Id: c_curved.c,v 1.1 1997-04-11 17:41:33 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_curved
#ifdef NeedFuncProto
(
    float *x,
    float *y,
    int n
)
#else
(x,y,n)
    float *x;
    float *y;
    int n;
#endif
{
    NGCALLF(curved,CURVED)(x,y,&n);
}
