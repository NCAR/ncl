/*
 *	$Id: c_kumx.c,v 1.1 1997-04-11 17:44:24 haley Exp $
 */
#include <ncarg/ncargC.h>

int c_kumx
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
	return(NGCALLF(kumx,KUMX)(&rx2));
}
