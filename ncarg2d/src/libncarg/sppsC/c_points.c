/*
 *	$Id: c_points.c,v 1.1 1997-04-11 17:44:28 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_points
#ifdef NeedFuncProto
(
    float *px,
    float *py,
    int np,
    int ic,
    int il
)
#else
(px,py,np,ic,il)
    float *px;
    float *py;
    int np;
    int ic;
    int il;
#endif
{
    NGCALLF(points,POINTS)(px,py,&np,&ic,&il);
}
