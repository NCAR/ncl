/*
 *	$Id: c_mstrms.c,v 1.1 1997-04-11 17:45:00 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_mstrms 
#ifdef NeedFuncProto
(
    float *diag,
    float *sdiag,
    float sigma,
    float del
)
#else
(diag,sdiag,sigma,del)
    float *diag;
    float *sdiag;
    float sigma;
    float del;
#endif
{
    float sigma2, del2;
    sigma2 = sigma;
    del2 = del;
    NGCALLF(mstrms,MSTRMS)(diag,sdiag,&sigma2,&del2);
}
