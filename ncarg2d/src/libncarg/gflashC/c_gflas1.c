/*
 *	$Id: c_gflas1.c,v 1.1 1997-04-11 17:42:29 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_gflas1
#ifdef NeedFuncProto
(
    int iname
)
#else
(iname)
    int iname;
#endif
{
   NGCALLF(gflas1,GFLAS1)(&iname);
}
