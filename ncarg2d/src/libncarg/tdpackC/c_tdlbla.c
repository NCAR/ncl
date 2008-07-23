/*
 *      $Id: c_tdlbla.c,v 1.5 2008-07-23 16:17:06 haley Exp $
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

extern void NGCALLF(tdlbla,TDLBLA)(int*,NGstring,NGstring,float*,float*,
                                   float*,float*,float*,int,int);

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
    int loilbl;
    NGstring ilbl2;
    int lonlbl;
    NGstring nlbl2;
    loilbl=NGSTRLEN(ilbl);
    ilbl2=NGCstrToFstr(ilbl,loilbl);
    lonlbl=NGSTRLEN(nlbl);
    nlbl2=NGCstrToFstr(nlbl,lonlbl);
    NGCALLF(tdlbla,TDLBLA)(&iaxs,ilbl2,nlbl2,&xat0,&xat1,&yat0,&yat1,
                                                 &angd,loilbl,lonlbl);
}
