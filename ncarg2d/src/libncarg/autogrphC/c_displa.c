/*
 *	$Id: c_displa.c,v 1.1 1997-04-11 17:40:40 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_displa
#ifdef NeedFuncProto
(
    int lfra,
    int lrow,
    int ltyp
)
#else
(lfra,lrow,ltyp)
    int lfra;
    int lrow;
    int ltyp;
#endif
{
    NGCALLF(displa,DISPLA)(&lfra,&lrow,&ltyp);
}
