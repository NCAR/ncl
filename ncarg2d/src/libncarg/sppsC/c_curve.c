/*
 *	$Id: c_curve.c,v 1.1 1997-04-11 17:44:18 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_curve
#ifdef NeedFuncProto
(
    float *px,
    float *py,
    int np
)
#else
(px,py,np)
    float *px;
    float *py;
    int np;
#endif
{
    NGCALLF(curve,CURVE)(px,py,&np);
}
