/*
 *	$Id: c_gflas3.c,v 1.1 1997-04-11 17:42:30 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_gflas3
#ifdef NeedFuncProto
(
    int iname
)
#else
(iname)
    int iname;
#endif
{
    NGCALLF(gflas3,GFLAS3)(&iname);
}
