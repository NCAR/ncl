/*
 *	$Id: c_frstd.c,v 1.1 1997-04-11 17:41:35 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_frstd
#ifdef NeedFuncProto
(
    float x,
    float y
)
#else
(x,y)
    float x;
    float y;
#endif
{
    float x2, y2;
    x2 = x;
    y2 = y;
    NGCALLF(frstd,FRSTD)(&x2,&y2);
}
