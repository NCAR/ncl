/*
 *      $Id: c_tdlbls.c,v 1.1 1997-06-30 21:47:35 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdlbls
#ifdef NeedFuncProto
(
    float umin,
    float vmin,
    float wmin,
    float umax,
    float vmax,
    float wmax,
    char* unlb,
    char* vnlb,
    char* wnlb,
    char* uilb,
    char* vilb,
    char* wilb,
    int   ipck
)
#else
(umin,vmin,wmin,umax,vmax,wmax,unlb,vnlb,wnlb,uilb,vilb,wilb,ipck)
    float umin;
    float vmin;
    float wmin;
    float umax;
    float vmax;
    float wmax;
    char* unlb;
    char* vnlb;
    char* wnlb;
    char* uilb;
    char* vilb;
    char* wilb;
    int   ipck;
#endif
{
    float umin2,vmin2,wmin2,umax2,vmax2,wmax2;
    int lounlb;
    NGstring unlb2;
    int lovnlb;
    NGstring vnlb2;
    int lownlb;
    NGstring wnlb2;
    int louilb;
    NGstring uilb2;
    int lovilb;
    NGstring vilb2;
    int lowilb;
    NGstring wilb2;
    int   ipck2;
    umin2=umin;
    vmin2=vmin;
    wmin2=wmin;
    umax2=umax;
    vmax2=vmax;
    wmax2=wmax;
    lounlb=NGSTRLEN(unlb);
    unlb2=NGCstrToFstr(unlb,lounlb);
    lovnlb=NGSTRLEN(vnlb);
    vnlb2=NGCstrToFstr(vnlb,lovnlb);
    lownlb=NGSTRLEN(wnlb);
    wnlb2=NGCstrToFstr(wnlb,lownlb);
    louilb=NGSTRLEN(uilb);
    uilb2=NGCstrToFstr(uilb,louilb);
    lovilb=NGSTRLEN(vilb);
    vilb2=NGCstrToFstr(vilb,lovilb);
    lowilb=NGSTRLEN(wilb);
    wilb2=NGCstrToFstr(wilb,lowilb);
    ipck2=ipck;
    NGCALLF(tdlbls,TDLBLS)(&umin2,&vmin2,&wmin2,&umax2,&vmax2,&wmax2,
                          unlb2,vnlb2,wnlb2,uilb2,vilb2,wilb2,&ipck2,
                           lounlb,lovnlb,lownlb,louilb,lovilb,lowilb);
}
