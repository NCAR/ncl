/*
 *      $Id: c_tdgrds.c,v 1.1 1997-06-30 21:47:28 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdgrds
#ifdef NeedFuncProto
(
    float umin,
    float vmin,
    float wmin,
    float umax,
    float vmax,
    float wmax,
    float ustp,
    float vstp,
    float wstp,
    int   igrt,
    int   ihid
)
#else
(umin,vmin,wmin,umax,vmax,wmax,ustp,vstp,wstp,igrt,ihid)
    float umin;
    float vmin;
    float wmin;
    float umax;
    float vmax;
    float wmax;
    float ustp;
    float vstp;
    float wstp;
    int   igrt;
    int   ihid;
#endif
{
    float umin2,vmin2,wmin2,umax2,vmax2,wmax2,ustp2,vstp2,wstp2;
    int   igrt2,ihid2;
    umin2=umin;
    vmin2=vmin;
    wmin2=wmin;
    umax2=umax;
    vmax2=vmax;
    wmax2=wmax;
    ustp2=ustp;
    vstp2=vstp;
    wstp2=wstp;
    igrt2=igrt;
    ihid2=ihid;
    NGCALLF(tdgrds,TDGRDS)(&umin2,&vmin2,&wmin2,&umax2,&vmax2,&wmax2,
                                  &ustp2,&vstp2,&wstp2,&igrt2,&ihid2);
}
