/*
 *	$Id: c_vvectr.c,v 1.1 1997-04-11 17:45:16 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_vvectr
#ifdef NeedFuncProto
(
    float *u,
    float *v,
    float *p,
    int *iam,
    int (*vvudmv_)(
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai
    ),
    float *wrk
)
#else
(u,v,p,iam,vvudmv_,wrk)
    float *u;
    float *v;
    float *p;
    int *iam;
    int (*vvudmv_)();
    float *wrk;
#endif
{
    NGCALLF(vvectr,VVECTR)(u,v,p,iam,vvudmv_,wrk);
}
