/*
 *	$Id: c_kfpx.c,v 1.1 1997-04-11 17:44:22 haley Exp $
 */
#include <ncarg/ncargC.h>

int c_kfpx
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
	return(NGCALLF(kfpx,KFPX)(&rx2));
}
