/*
 *	$Id: c_cpux.c,v 1.1 1997-04-11 17:44:17 haley Exp $
 */
#include <ncarg/ncargC.h>

float c_cpux
#ifdef NeedFuncProto
(
    int ix
)
#else
(ix)
    int ix;
#endif
{
    float x;
    x = NGCALLF(cpux,CPUX)(&ix);
    return(x);
}
