/*
 *      $Id: c_mdgcog.c,v 1.2 2008-07-23 16:16:49 haley Exp $
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

extern void NGCALLF(mdgcog,MDGCOG)(double*,double*,double*,double*,
				   double*,int*);

void c_mdgcog
#ifdef NeedFuncProto
(
    double clat,
    double clon,
    double crad,
    double *alat,
    double *alon,
    int npts
)
#else
 (clat,clon,crad,alat,alon,npts)
    double clat;
    double clon;
    double crad;
    double *alat;
    double *alon;
    int npts;
#endif
{
    NGCALLF(mdgcog,MDGCOG)(&clat,&clon,&crad,alat,alon,&npts);
}
