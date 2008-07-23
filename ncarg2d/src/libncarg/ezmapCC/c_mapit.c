/*
 *	$Id: c_mapit.c,v 1.2 2008-07-23 16:16:47 haley Exp $
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

extern void NGCALLF(mapit,MAPIT)(float*,float*,int*);

void c_mapit
#ifdef NeedFuncProto
(
    float xlat,
    float xlon,
    int ifst
)
#else
(xlat,xlon,ifst)
    float xlat;
    float xlon;
    int ifst;
#endif
{
      NGCALLF(mapit,MAPIT)(&xlat,&xlon,&ifst);
}
