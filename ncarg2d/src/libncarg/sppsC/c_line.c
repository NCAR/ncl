/*
 *	$Id: c_line.c,v 1.1 1997-04-11 17:44:25 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_line
#ifdef NeedFuncProto
(
    float x1,
    float y1,
    float x2,
    float y2
)
#else
(x1,y1,x2,y2)
    float x1;
    float y1;
    float x2;
    float y2;
#endif
{
    float u1,v1,u2,v2;
    u1 = x1;
    v1 = y1;
    u2 = x2;
    v2 = y2;
    NGCALLF(line,LINE)(&u1,&v1,&u2,&v2);
}
