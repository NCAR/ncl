/*
 *	$Id: c_line3.c,v 1.1 1997-04-11 17:45:09 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_line3 
#ifdef NeedFuncProto
(
    float ua,
    float va,
    float wa,
    float ub,
    float vb,
    float wb
)
#else
 (ua,va,wa,ub,vb,wb)
    float ua;
    float va;
    float wa;
    float ub;
    float vb;
    float wb;
#endif
{
    float ua2,va2,wa2,ub2,vb2,wb2;
    ua2 = ua;
    va2 = va;
    wa2 = wa;
    ub2 = ub;
    vb2 = vb;
    wb2 = wb;
    NGCALLF(line3,LINE3)(&ua2,&va2,&wa2,&ub2,&vb2,&wb2);
}
