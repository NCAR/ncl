/*
 *	$Id: c_cmfy.c,v 1.1 1997-04-11 17:44:15 haley Exp $
 */
#include <ncarg/ncargC.h>

float c_cmfy
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
    y = NGCALLF(cmfy,CMFY)(&iy);
    return(y);
}
