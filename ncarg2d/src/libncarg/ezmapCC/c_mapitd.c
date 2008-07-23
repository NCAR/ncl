/*
 *      $Id: c_mapitd.c,v 1.2 2008-07-23 16:16:47 haley Exp $
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

extern void NGCALLF(mapitd,MAPITD)(float*,float*,int*);

void c_mapitd
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
      NGCALLF(mapitd,MAPITD)(&xlat,&xlon,&ifst);
}
