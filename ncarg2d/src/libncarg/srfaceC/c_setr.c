/*
 *  $Id: c_setr.c,v 1.1 1997-04-11 17:44:33 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_setr
#ifdef NeedFuncProto
(
    float xmin,
    float xmax,
    float ymin,
    float ymax,
    float zmin,
    float zmax,
    float r0
)
#else
(xmin,xmax,ymin,ymax,zmin,zmax,r0)
    float xmin;
    float xmax;
    float ymin;
    float ymax;
    float zmin;
    float zmax;
    float r0;
#endif
{
    NGCALLF(setr,SETR)(&xmin,&xmax,&ymin,&ymax,&zmin,&zmax,&r0);
}
