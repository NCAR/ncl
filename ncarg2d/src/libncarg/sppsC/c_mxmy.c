/*
 *	$Id: c_mxmy.c,v 1.1 1997-04-11 17:44:26 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_mxmy
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
    NGCALLF(mxmy,MXMY)(ix,iy);
}
