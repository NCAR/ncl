/*
 *	$Id: c_kumy.c,v 1.1 1997-04-11 17:44:24 haley Exp $
 */
#include <ncarg/ncargC.h>

int c_kumy
#ifdef NeedFuncProto
(
    float ry
)
#else
(ry)
    float ry;
#endif
{
	float ry2;
	ry2 = ry;
	return(NGCALLF(kumy,KUMY)(&ry2));
}
