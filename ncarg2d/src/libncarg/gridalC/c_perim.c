/*
 *	$Id: c_perim.c,v 1.1 1997-04-11 17:43:10 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_perim
#ifdef NeedFuncProto
(
    int mjrx,
    int mnrx,
    int mjry,
    int mnry
)
#else
(mjrx,mnrx,mjry,mnry)
    int mjrx;
    int mnrx;
    int mjry;
    int mnry;
#endif
{
    NGCALLF(perim,PERIM)(&mjrx,&mnrx,&mjry,&mnry);
}
