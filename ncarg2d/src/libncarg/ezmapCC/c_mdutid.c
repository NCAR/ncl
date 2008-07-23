/*
 *      $Id: c_mdutid.c,v 1.2 2008-07-23 16:16:52 haley Exp $
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

extern void NGCALLF(mdutid,MDUTID)(double*,double*,double*,double*);

void c_mdutid
#ifdef NeedFuncProto
(
    double uval,
    double vval,
    double *rlat,
    double *rlon
)
#else
(uval,vval,rlat,rlon)
    double uval;
    double vval;
    double *rlat;
    double *rlon;
#endif
{
    NGCALLF(mdutid,MDUTID)(&uval,&vval,rlat,rlon);
}
