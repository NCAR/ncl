/*
 *	$Id: c_seti.c,v 1.1 1997-04-11 17:44:29 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_seti
#ifdef NeedFuncProto
(
    int ix,
    int iy 
)
#else
(ix,iy)
    int ix;
    int iy;
#endif
{
    NGCALLF(seti,SETI)(&ix,&iy);
}
