/*
 *	$Id: c_velvct.c,v 1.1 1997-04-11 17:45:15 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_velvct
#ifdef NeedFuncProto
(
    float *u,
    int lu,
    float *v,
    int lv,
    int m,
    int n,
    float flo,
    float hi,
    int nset,
    int length,
    int ispv,
    float *spv
)
#else
(u,lu,v,lv,m,n,flo,hi,nset,length,ispv,spv)
    float *u;
    int lu;
    float *v;
    int lv;
    int m;
    int n;
    float flo;
    float hi;
    int nset;
    int length;
    int ispv;
    float *spv;
#endif
{
    float flo2,hi2;

    flo2 = flo;
    hi2 = hi;

    NGCALLF(velvct,VELVCT)(u,&lu,v,&lv,&m,&n,&flo2,&hi2,&nset,&length,&ispv,spv);
}
