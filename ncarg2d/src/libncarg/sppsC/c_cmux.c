/*
 *	$Id: c_cmux.c,v 1.1 1997-04-11 17:44:15 haley Exp $
 */
#include <ncarg/ncargC.h>

float c_cmux
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
    x = NGCALLF(cmux,CMUX)(&ix);
    return(x);
}
