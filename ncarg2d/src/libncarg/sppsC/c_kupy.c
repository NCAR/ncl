/*
 *	$Id: c_kupy.c,v 1.1 1997-04-11 17:44:25 haley Exp $
 */
#include <ncarg/ncargC.h>

int c_kupy
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
	return(NGCALLF(kupy,KUPY)(&ry2));
}
