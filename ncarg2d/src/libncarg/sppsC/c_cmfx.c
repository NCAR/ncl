/*
 *	$Id: c_cmfx.c,v 1.1 1997-04-11 17:44:14 haley Exp $
 */
#include <ncarg/ncargC.h>

float c_cmfx
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
    x = NGCALLF(cmfx,CMFX)(&ix);
    return(x);
}
