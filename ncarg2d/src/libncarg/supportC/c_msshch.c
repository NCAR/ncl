/*
 *	$Id: c_msshch.c,v 1.1 1997-04-11 17:44:59 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_msshch 
#ifdef NeedFuncProto
(
    float *sinhm,
    float *coshm,
    float x,
    int isw
)
#else
(sinhm,coshm,x,isw)
    float *sinhm;
    float *coshm;
    float x;
    int isw;
#endif
{
    float x2;
    x2 = x;
    NGCALLF(msshch,MSSHCH)(sinhm,coshm,&x2,&isw);
}
