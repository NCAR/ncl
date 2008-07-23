/*
 *      $Id: c_mdptra.c,v 1.2 2008-07-23 16:16:51 haley Exp $
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

extern void NGCALLF(mdptra,MDPTRA)(double*,double*,double*,double*);

void c_mdptra
#ifdef NeedFuncProto
(
    double rlat,
    double rlon,
    double *uval,
    double *vval
)
#else
(rlat,rlon,uval,vval)
    double rlat;
    double rlon;
    double *uval;
    double *vval;
#endif
{
    NGCALLF(mdptra,MDPTRA)(&rlat,&rlon,uval,vval);
}
