/*
 *	$Id: c_set.c,v 1.1 1997-04-11 17:44:28 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_set
#ifdef NeedFuncProto
(
    float vl,
    float vr,
    float vb,
    float vt,
    float wl,
    float wr,
    float wb,
    float wt,
    int lf
)
#else
(vl,vr,vb,vt,wl,wr,wb,wt,lf)
    float vl;
    float vr;
    float vb;
    float vt;
    float wl;
    float wr;
    float wb;
    float wt;
    int lf;
#endif
{

    float vl2,vr2,vb2,vt2,wl2,wr2,wb2,wt2;
    vl2 = vl;
    vb2 = vb;
    wl2 = wl;
    wb2 = wb;
    vr2 = vr;
    vt2 = vt;
    wr2 = wr;
    wt2 = wt;
    NGCALLF(set,SET)(&vl2,&vr2,&vb2,&vt2,&wl2,&wr2,&wb2,&wt2,&lf);
}
