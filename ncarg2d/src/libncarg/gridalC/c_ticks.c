/*
 *	$Id: c_ticks.c,v 1.1 1997-04-11 17:43:12 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ticks
#ifdef NeedFuncProto
(
    int lmjr,
    int lmnr
)
#else
(lmjr,lmnr)
    int lmjr;
    int lmnr;
#endif
{
    NGCALLF(ticks,TICKS)(&lmjr,&lmnr);
}
