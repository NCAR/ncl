/*
 *	$Id: c_dpcurv.c,v 1.1 1997-04-11 17:41:41 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_dpcurv
#ifdef NeedFuncProto
(
    float *xcpu,
    float *ycpu,
    int npts
)
#else
(xcpu,ycpu,npts)
    float *xcpu;
    float *ycpu;
    int npts;
#endif
{
    NGCALLF(dpcurv,DPCURV)(xcpu,ycpu,&npts);
}
