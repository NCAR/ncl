/*
 *	$Id: c_cmuy.c,v 1.1 1997-04-11 17:44:16 haley Exp $
 */
#include <ncarg/ncargC.h>

float c_cmuy
#ifdef NeedFuncProto
(
    int iy
)
#else
(iy)
    int iy;
#endif
{
    float y;
    y = NGCALLF(cmuy,CMUY)(&iy);
    return(y);
}
