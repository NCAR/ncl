/*
 *	$Id: c_cfux.c,v 1.1 1997-04-11 17:44:13 haley Exp $
 */
#include <ncarg/ncargC.h>

float c_cfux
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
    x = NGCALLF(cfux,CFUX)(&rx2);
    return(x);
}
