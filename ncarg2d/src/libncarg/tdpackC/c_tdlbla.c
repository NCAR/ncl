/*
 *      $Id: c_tdlbla.c,v 1.1 1997-06-30 21:47:34 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdlbla
#ifdef NeedFuncProto
(
    int   iaxs,
    char* ilbl,
    char* nlbl,
    float xat0,
    float xat1,
    float yat0,
    float yat1,
    float angd
)
#else
(iaxs,ilbl,nlbl,xat0,xat1,yat0,yat1,angd)
    int   iaxs;
    char* ilbl;
    char* nlbl;
    float xat0;
    float xat1;
    float yat0;
    float yat1;
    float angd;
#endif
{
    int iaxs2;
    int loilbl;
    NGstring ilbl2;
    int lonlbl;
    NGstring nlbl2;
    float xat02,xat12,yat02,yat12,angd2;
    iaxs2=iaxs;
    loilbl=NGSTRLEN(ilbl);
    ilbl2=NGCstrToFstr(ilbl,loilbl);
    lonlbl=NGSTRLEN(nlbl);
    nlbl2=NGCstrToFstr(nlbl,lonlbl);
    xat02=xat0;
    xat12=xat1;
    yat02=yat0;
    yat12=yat1;
    angd2=angd;
    NGCALLF(tdlbla,TDLBLA)(&iaxs2,ilbl2,nlbl2,&xat02,&xat12,&yat02,&yat12,
                                                     &angd2,loilbl,lonlbl);
}
