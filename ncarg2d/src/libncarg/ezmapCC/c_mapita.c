/*
 *	$Id: c_mapita.c,v 1.2 2008-07-23 16:16:47 haley Exp $
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

extern void NGCALLF(mapita,MAPITA)(float*,float*,int*,int*,int*,int*,int*);

void c_mapita
#ifdef NeedFuncProto
(
    float xlat,
    float xlon,
    int ifst,
    int *iamp,
    int igrp,
    int idlt,
    int idrt
)
#else
(xlat,xlon,ifst,iamp,igrp,idlt,idrt)
    float xlat;
    float xlon;
    int ifst;
    int *iamp;
    int igrp;
    int idlt;
    int idrt;
#endif
{
    NGCALLF(mapita,MAPITA)(&xlat,&xlon,&ifst,iamp,&igrp,&idlt,&idrt);
}
