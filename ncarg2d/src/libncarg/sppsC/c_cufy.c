/*
 *	$Id: c_cufy.c,v 1.1 1997-04-11 17:44:18 haley Exp $
 */
#include <ncarg/ncargC.h>

float c_cufy
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
    y = NGCALLF(cufy,CUFY)(&ry2);
    return(y);
}
