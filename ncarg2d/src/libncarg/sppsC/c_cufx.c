/*
 *	$Id: c_cufx.c,v 1.1 1997-04-11 17:44:18 haley Exp $
 */
#include <ncarg/ncargC.h>

float c_cufx
#ifdef NeedFuncProto
(
    float rx
)
#else
(rx)
    float rx;
#endif
{
    float rx2,x;
    rx2 = rx;
    x = NGCALLF(cufx,CUFX)(&rx2);
    return(x);
}
