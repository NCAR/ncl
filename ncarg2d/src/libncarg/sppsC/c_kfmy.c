/*
 *	$Id: c_kfmy.c,v 1.1 1997-04-11 17:44:21 haley Exp $
 */
#include <ncarg/ncargC.h>

int c_kfmy
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
	return(NGCALLF(kfmy,KFMY)(&ry2));
}
