/*
 *	$Id: c_dpline.c,v 1.1 1997-04-11 17:41:45 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_dpline
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
    float xcp12, ycp12;
    float xcp22, ycp22;

    xcp12 = xcp1;
    ycp12 = ycp1;
    xcp22 = xcp2;
    ycp22 = ycp2;
    NGCALLF(dpline,DPLINE)(&xcp12,&ycp12,&xcp22,&ycp22);
}
