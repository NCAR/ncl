/*
 *	$Id: c_agcurv.c,v 1.1 1997-04-11 17:40:30 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_agcurv
#ifdef NeedFuncProto
(
    float *xvec,
    int iiex,
    float *yvec,
    int iiey,
    int nexy,
    int kdsh
)
#else
(xvec,iiex,yvec,iiey,nexy,kdsh)
    float *xvec;
    int iiex;
    float *yvec;
    int iiey;
    int nexy;
    int kdsh;
#endif
{
    NGCALLF(agcurv,AGCURV)(xvec,&iiex,yvec,&iiey,&nexy,&kdsh);
}
