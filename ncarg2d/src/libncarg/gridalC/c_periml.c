/*
 *	$Id: c_periml.c,v 1.1 1997-04-11 17:43:11 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_periml
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
    NGCALLF(periml,PERIML)(&mjrx,&mnrx,&mjry,&mnry);
}
