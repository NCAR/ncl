/*
 *      $Id: c_tdline.c,v 1.1 1997-06-30 21:47:36 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdline
#ifdef NeedFuncProto
(
    float ucp1,
    float vcp1,
    float wcp1,
    float ucp2,
    float vcp2,
    float wcp2
)
#else
(ucp1,vcp1,wcp1,ucp2,vcp2,wcp2)
    float ucp1;
    float vcp1;
    float wcp1;
    float ucp2;
    float vcp2;
    float wcp2;
#endif
{
    float ucp12,vcp12,wcp12,ucp22,vcp22,wcp22;
    ucp12=ucp1;
    vcp12=vcp1;
    wcp12=wcp1;
    ucp22=ucp2;
    vcp22=vcp2;
    wcp22=wcp2;
    NGCALLF(tdline,TDLINE)(&ucp12,&vcp12,&wcp12,&ucp22,&vcp22,&wcp22);
}
