/*
 *	$Id: c_cpcltr.c,v 1.1 1997-04-11 17:41:02 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_cpcltr
#ifdef NeedFuncProto
(
    float *zdat,
    float *rwrk,
    int *iwrk,
    float clvl,
    int *ijmp,
    int *irw1,
    int *irw2,
    int *nrwk
)
#else
 (zdat,rwrk,iwrk,clvl,ijmp,irw1,irw2,nrwk)
    float *zdat;
    float *rwrk;
    int *iwrk;
    float clvl;
    int *ijmp;
    int *irw1;
    int *irw2;
    int *nrwk;
#endif
{
    float clvl2;
    clvl2 = clvl;
    NGCALLF(cpcltr,CPCLTR)(zdat,rwrk,iwrk,&clvl2,ijmp,irw1,irw2,nrwk);
}
