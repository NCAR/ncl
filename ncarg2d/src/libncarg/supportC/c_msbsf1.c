/*
 *	$Id: c_msbsf1.c,v 1.1 1997-04-11 17:44:56 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_msbsf1 
#ifdef NeedFuncProto
(
    int m,
    int n,
    float xmin,
    float xmax,
    float ymin,
    float ymax,
    float *z,
    int iz,
    float *zp,
    float *temp,
    float sigma
)
#else
(m,n,xmin,xmax,ymin,ymax,z,iz,zp,temp,sigma)
    int m;
    int n;
    float xmin;
    float xmax;
    float ymin;
    float ymax;
    float *z;
    int iz;
    float *zp;
    float *temp;
    float sigma;
#endif
{
    float xmin2, xmax2, ymin2, ymax2, sigma2;

    xmin2 = xmin;
    xmax2 = xmax;
    ymin2 = ymin;
    ymax2 = ymax;
    sigma2 = sigma;
    
    NGCALLF(msbsf1,MSBSF1)(&m,&n,&xmin2,&xmax2,&ymin2,&ymax2,z,&iz,zp,temp,&sigma2);
}
