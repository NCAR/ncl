/*
 *	$Id: c_cfuy.c,v 1.1 1997-04-11 17:44:14 haley Exp $
 */
#include <ncarg/ncargC.h>

float c_cfuy
#ifdef NeedFuncProto
(
    float ry
)
#else
(ry)
    float ry;
#endif
{
    float ry2,y;
    ry2 = ry;
    y = NGCALLF(cfuy,CFUY)(&ry2);
    return(y);
}
