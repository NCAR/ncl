/*
 *      $Id: c_mdpita.c,v 1.2 2008-07-23 16:16:50 haley Exp $
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

extern void NGCALLF(mdpita,MDPITA)(double*,double*,int*,int*,int*,int*,int*);

void c_mdpita
#ifdef NeedFuncProto
(
    double xlat,
    double xlon,
    int ifst,
    int *iamp,
    int igrp,
    int idlt,
    int idrt
)
#else
(xlat,xlon,ifst,iamp,igrp,idlt,idrt)
    double xlat;
    double xlon;
    int ifst;
    int *iamp;
    int igrp;
    int idlt;
    int idrt;
#endif
{
    NGCALLF(mdpita,MDPITA)(&xlat,&xlon,&ifst,iamp,&igrp,&idlt,&idrt);
}
