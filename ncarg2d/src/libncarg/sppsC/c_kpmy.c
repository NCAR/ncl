/*
 *	$Id: c_kpmy.c,v 1.1 1997-04-11 17:44:24 haley Exp $
 */
#include <ncarg/ncargC.h>

int c_kpmy
#ifdef NeedFuncProto
(
    int iy
)
#else
(iy)
    int iy;
#endif
{
	return(NGCALLF(kpmy,KPMY)(&iy));
}
