/*
 *      $Id: c_tdinit.c,v 1.1 1997-06-30 21:47:31 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdinit
#ifdef NeedFuncProto
(
    float xmid,
    float ymid,
    float zmid,
    float xori,
    float yori,
    float zori,
    float xthi,
    float ythi,
    float zthi,
    float otep
)
#else
(xmid,ymid,zmid,xori,yori,zori,xthi,ythi,zthi,otep)
    float xmid;
    float ymid;
    float zmid;
    float xori;
    float yori;
    float zori;
    float xthi;
    float ythi;
    float zthi;
    float otep;
#endif
{
    float xmid2,ymid2,zmid2,xori2,yori2,zori2,xthi2,ythi2,zthi2,otep2;
    xmid2=xmid;
    ymid2=ymid;
    zmid2=zmid;
    xori2=xori;
    yori2=yori;
    zori2=zori;
    xthi2=xthi;
    ythi2=ythi;
    zthi2=zthi;
    otep2=otep;
    NGCALLF(tdinit,TDINIT)(&xmid2,&ymid2,&zmid2,&xori2,&yori2,&zori2,
                                                &xthi2,&ythi2,&zthi2,&otep2);
}
