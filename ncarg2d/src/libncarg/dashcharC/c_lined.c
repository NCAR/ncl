/*
 *	$Id: c_lined.c,v 1.1 1997-04-11 17:41:36 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_lined
#ifdef NeedFuncProto
(
    float xa,
    float ya,
    float xb,
    float yb
)
#else
(xa,ya,xb,yb)
    float xa;
    float ya;
    float xb;
    float yb;
#endif
{
    float xa2,ya2,xb2,yb2;
    xa2 = xa;
    ya2 = ya;
    xb2 = xb;
    yb2 = yb;
    NGCALLF(lined,LINED)(&xa2,&ya2,&xb2,&yb2);
}
