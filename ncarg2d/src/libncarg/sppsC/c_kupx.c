/*
 *	$Id: c_kupx.c,v 1.1 1997-04-11 17:44:25 haley Exp $
 */
#include <ncarg/ncargC.h>

int c_kupx
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
	return(NGCALLF(kupx,KUPX)(&rx2));
}
