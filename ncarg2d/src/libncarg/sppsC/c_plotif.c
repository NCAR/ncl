/*
 *	$Id: c_plotif.c,v 1.1 1997-04-11 17:44:27 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_plotif
#ifdef NeedFuncProto
(
    float fx,
    float fy,
    int ip
)
#else
(fx,fy,ip)
    float fx;
    float fy;
    int ip;
#endif
{
    float fx2, fy2;
    fx2 = fx;
    fy2 = fy;
    NGCALLF(plotif,PLOTIF)(&fx2,&fy2,&ip);
}
