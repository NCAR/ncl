/*
 *	$Id: c_kfpy.c,v 1.1 1997-04-11 17:44:22 haley Exp $
 */
#include <ncarg/ncargC.h>

int c_kfpy
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
	return(NGCALLF(kfpy,KFPY)(&ry2));
}
