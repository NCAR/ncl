/*
 *      $Id: c_tdinit.c,v 1.2 1997-07-02 22:26:57 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdinit
#ifdef NeedFuncProto
(
    float umid,
    float vmid,
    float wmid,
    float uori,
    float vori,
    float wori,
    float uthi,
    float vthi,
    float wthi,
    float otep
)
#else
(umid,vmid,wmid,uori,vori,wori,uthi,vthi,wthi,otep)
    float umid;
    float vmid;
    float wmid;
    float uori;
    float vori;
    float wori;
    float uthi;
    float vthi;
    float wthi;
    float otep;
#endif
{
    float umid2,vmid2,wmid2,uori2,vori2,wori2,uthi2,vthi2,wthi2,otep2;
    umid2=umid;
    vmid2=vmid;
    wmid2=wmid;
    uori2=uori;
    vori2=vori;
    wori2=wori;
    uthi2=uthi;
    vthi2=vthi;
    wthi2=wthi;
    otep2=otep;
    NGCALLF(tdinit,TDINIT)(&umid2,&vmid2,&wmid2,&uori2,&vori2,&wori2,
                                                &uthi2,&vthi2,&wthi2,&otep2);
}
