/*
 *	$Id: c_kmpy.c,v 1.1 1997-04-11 17:44:23 haley Exp $
 */
#include <ncarg/ncargC.h>

int c_kmpy
#ifdef NeedFuncProto
(
    int iy
)
#else
(iy)
    int iy;
#endif
{
	return(NGCALLF(kmpy,KMPY)(&iy));
}
