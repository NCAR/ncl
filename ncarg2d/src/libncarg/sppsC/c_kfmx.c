/*
 *	$Id: c_kfmx.c,v 1.1 1997-04-11 17:44:21 haley Exp $
 */
#include <ncarg/ncargC.h>

int c_kfmx
#ifdef NeedFuncProto
(
    float rx
)
#else
(rx)
    float rx;
#endif
{
	float rx2;
	rx2 = rx;
	return(NGCALLF(kfmx,KFMX)(&rx2));
}
