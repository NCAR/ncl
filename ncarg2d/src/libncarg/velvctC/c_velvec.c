/*
 *	$Id: c_velvec.c,v 1.1 1997-04-11 17:45:16 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_velvec
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
    int ispv,
    float *spv
)
#else
(u,lu,v,lv,m,n,flo,hi,nset,ispv,spv)
    float *u;
    int lu;
    float *v;
    int lv;
    int m;
    int n;
    float flo;
    float hi;
    int nset;
    int ispv;
    float *spv;
#endif
{
    float flo2,hi2;

    flo2 = flo;
    hi2 = hi;

    NGCALLF(velvec,VELVEC)(u,&lu,v,&lv,&m,&n,&flo2,&hi2,&nset,&ispv,spv);
}
