/*
 *	$Id: c_msceez.c,v 1.1 1997-04-11 17:44:57 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_msceez 
#ifdef NeedFuncProto
(
    float del1,
    float del2,
    float sigma,
    float *c1,
    float *c2,
    float *c3,
    int n
)
#else
(del1,del2,sigma,c1,c2,c3,n)
    float del1;
    float del2;
    float sigma;
    float *c1;
    float *c2;
    float *c3;
    int n;
#endif
{
    float del12, del22, sigma2;

    del12 = del1;
    del22 = del2;
    sigma2 = sigma;

    NGCALLF(msceez,MSCEEZ)(&del12,&del22,&sigma2,c1,c2,c3,&n);
}
