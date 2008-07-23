/*
 *      $Id: c_mdpit.c,v 1.2 2008-07-23 16:16:50 haley Exp $
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

extern void NGCALLF(mdpit,MDPIT)(double*,double*,int*);

void c_mdpit
#ifdef NeedFuncProto
(
    double xlat,
    double xlon,
    int ifst
)
#else
(xlat,xlon,ifst)
    double xlat;
    double xlon;
    int ifst;
#endif
{
      NGCALLF(mdpit,MDPIT)(&xlat,&xlon,&ifst);
}
