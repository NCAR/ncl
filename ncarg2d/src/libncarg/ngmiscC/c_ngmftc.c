/*
 *	$Id: c_ngmftc.c,v 1.1 1997-04-11 17:43:40 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ngmftc
#ifdef NeedFuncProto
(
    int wkid
)
#else
(wkid)
    int wkid;
#endif
{
    NGCALLF(ngmftc,NGMFTC)(&wkid);
}
