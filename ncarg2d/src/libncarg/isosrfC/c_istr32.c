/*
 *	$Id: c_istr32.c,v 1.1 1997-04-11 17:43:24 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_istr32
#ifdef NeedFuncProto
(
    float ut,
    float vt,
    float wt,
    float xt,
    float yt,
    float zt,
    int ient
)
#else
(ut,vt,wt,xt,yt,zt,ient)
    float ut;
    float vt;
    float wt;
    float xt;
    float yt;
    float zt;
    int ient;
#endif
{
    float ut2,vt2,wt2,xt2,yt2,zt2;

    ut2 = ut;
    vt2 = vt;
    wt2 = wt;
    xt2 = xt;
    yt2 = yt;
    zt2 = zt;
    NGCALLF(istr32,ISTR32)(&ut2,&vt2,&wt2,&xt2,&yt2,&zt2,&ient);
}
