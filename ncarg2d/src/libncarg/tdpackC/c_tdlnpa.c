/*
 *      $Id: c_tdlnpa.c,v 1.1 1997-06-30 21:47:38 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdlnpa
#ifdef NeedFuncProto
(
    float xcp1,
    float ycp1,
    float xcp2,
    float ycp2
)
#else
(xcp1,ycp1,xcp2,ycp2)
    float xcp1;
    float ycp1;
    float xcp2;
    float ycp2;
#endif
{
    float xcp12,ycp12,xcp22,ycp22;
    xcp12=xcp1;
    ycp12=ycp1;
    xcp22=xcp2;
    ycp22=ycp2;
    NGCALLF(tdlnpa,TDLNPA)(&xcp12,&ycp12,&xcp22,&ycp22);
}
