/*
 *	$Id: c_tick4.c,v 1.1 1997-04-11 17:43:11 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_tick4
#ifdef NeedFuncProto
(
    int lmjx,
    int lmnx,
    int lmjy,
    int lmny
)
#else
(lmjx,lmnx,lmjy,lmny)
    int lmjx;
    int lmnx;
    int lmjy;
    int lmny;
#endif
{
    NGCALLF(tick4,TICK4)(&lmjx,&lmnx,&lmjy,&lmny);
}
