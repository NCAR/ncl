/*
 *	$Id: c_vect3.c,v 1.1 1997-04-11 17:45:11 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_vect3
#ifdef NeedFuncProto
(
    float u,
    float v,
    float w
)
#else
(u,v,w)
    float u;
    float v;
    float w;
#endif
{
    float u2,v2,w2;
    u2 = u;
    v2 = v;
    w2 = w;
    NGCALLF(vect3,VECT3)(&u2,&v2,&w2);
}
