/*
 *	$Id: c_agrstr.c,v 1.1 1997-04-11 17:40:35 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_agrstr
#ifdef NeedFuncProto
(
    int ifno
)
#else
(ifno)
    int ifno;
#endif
{
    NGCALLF(agrstr,AGRSTR)(&ifno);
}
