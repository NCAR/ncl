/*
 *	$Id: c_dashdb.c,v 1.1 1997-04-11 17:41:34 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_dashdb
#ifdef NeedFuncProto
(
    int *ipat
)
#else
(ipat)
    int *ipat;
#endif
{
    NGCALLF(dashdb,DASHDB)(ipat);
}
