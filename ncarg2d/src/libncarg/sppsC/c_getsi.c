/*
 *	$Id: c_getsi.c,v 1.1 1997-04-11 17:44:20 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_getsi
#ifdef NeedFuncProto
(
    int *ix,
    int *iy 
)
#else
(ix,iy)
    int *ix;
    int *iy;
#endif
{
    NGCALLF(getsi,GETSI)(ix,iy);
}
