/*
 *	$Id: c_plotit.c,v 1.1 1997-04-11 17:44:27 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_plotit
#ifdef NeedFuncProto
(
    int ix,
    int iy,
    int ip
)
#else
(ix,iy,ip)
    int ix;
    int iy;
    int ip;
#endif
{
    NGCALLF(plotit,PLOTIT)(&ix,&iy,&ip);
}
