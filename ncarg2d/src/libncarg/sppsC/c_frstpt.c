/*
 *	$Id: c_frstpt.c,v 1.1 1997-04-11 17:44:19 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_frstpt
#ifdef NeedFuncProto
(
    float px,
    float py
)
#else
(px,py)
    float px;
    float py;
#endif
{
    float px2, py2;
    px2 = px;
    py2 = py;
    NGCALLF(frstpt,FRSTPT)(&px2,&py2);
}
