/*
 *	$Id: c_cpfx.c,v 1.1 1997-04-11 17:44:16 haley Exp $
 */
#include <ncarg/ncargC.h>

float c_cpfx
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
    x = NGCALLF(cpfx,CPFX)(&ix);
    return(x);
}
