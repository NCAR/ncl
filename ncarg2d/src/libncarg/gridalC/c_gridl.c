/*
 *	$Id: c_gridl.c,v 1.1 1997-04-11 17:42:54 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_gridl
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
    NGCALLF(gridl,GRIDL)(&mjrx,&mnrx,&mjry,&mnry);
}
