/*
 *	$Id: c_grid.c,v 1.1 1997-04-11 17:42:53 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_grid
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
    NGCALLF(grid,GRID)(&mjrx,&mnrx,&mjry,&mnry);
}
