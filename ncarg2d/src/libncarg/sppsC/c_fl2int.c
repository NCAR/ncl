/*
 *	$Id: c_fl2int.c,v 1.1 1997-04-11 17:44:19 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_fl2int
#ifdef NeedFuncProto
(
    float px,
    float py,
    int *ix,
    int *iy
)
#else
(px,py,ix,iy)
    float px;
    float py;
    int *ix;
    int *iy;
#endif
{
    float px2, py2;
    px2 = px;
    py2 = py;
    NGCALLF(fl2int,FL2INT)(&px2,&py2,ix,iy);
}
