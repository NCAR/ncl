/*
 *	$Id: c_cpuy.c,v 1.1 1997-04-11 17:44:17 haley Exp $
 */
#include <ncarg/ncargC.h>

float c_cpuy
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
    y = NGCALLF(cpuy,CPUY)(&iy);
    return(y);
}
