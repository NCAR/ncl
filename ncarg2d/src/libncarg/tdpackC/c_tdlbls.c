/*
 *      $Id: c_tdlbls.c,v 1.5 2008-07-23 16:17:06 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <ncarg/ncargC.h>

extern void NGCALLF(tdlbls,TDLBLS)(float*,float*,float*,float*,float*,float*,
                                   NGstring,NGstring,NGstring,NGstring,
                                   NGstring,NGstring,int*,
                                   int,int,int,int,int,int);

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
    NGCALLF(tdlbls,TDLBLS)(&umin,&vmin,&wmin,&umax,&vmax,&wmax,
                            unlb2,vnlb2,wnlb2,uilb2,vilb2,wilb2,&ipck,
                           lounlb,lovnlb,lownlb,louilb,lovilb,lowilb);
}
