/*
 *	$Id: c_ezstrm.c,v 1.1 1997-04-11 17:44:48 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ezstrm
#ifdef NeedFuncProto
(
    float *u,
    float *v,
    float *work,
    int imax,
    int jmax
)
#else
(u,v,work,imax,jmax)
    float *u;
    float *v;
    float *work;
    int imax;
    int jmax;
#endif
{
    NGCALLF(ezstrm,EZSTRM)(u,v,work,&imax,&jmax);
}
