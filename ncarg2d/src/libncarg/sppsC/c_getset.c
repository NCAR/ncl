/*
 *	$Id: c_getset.c,v 1.1 1997-04-11 17:44:20 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_getset
#ifdef NeedFuncProto
(
    float *vl,
    float *vr,
    float *vb,
    float *vt,
    float *wl,
    float *wr,
    float *wb,
    float *wt,
    int *lf
)
#else
(vl,vr,vb,vt,wl,wr,wb,wt,lf)
    float *vl;
    float *vr;
    float *vb;
    float *vt;
    float *wl;
    float *wr;
    float *wb;
    float *wt;
    int *lf;
#endif
{
    NGCALLF(getset,GETSET)(vl,vr,vb,vt,wl,wr,wb,wt,lf);
}
