/*
 *	$Id: c_ezsrfc.c,v 1.1 1997-04-11 17:44:33 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ezsrfc
#ifdef NeedFuncProto
(
    float *z,
    int m,
    int n,
    float angh,
    float angv,
    float *work
)
#else
(z,m,n,angh,angv,work)
    float *z;
    int m;
    int n;
    float angh;
    float angv;
    float *work;
#endif
{
    float angh2, angv2;
    angh2 = angh;
    angv2 = angv;
    NGCALLF(ezsrfc,EZSRFC)(z,&m,&n,&angh2,&angv2,work);
}
