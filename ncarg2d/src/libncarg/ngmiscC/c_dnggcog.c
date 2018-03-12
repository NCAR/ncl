/************************************************************************
*                                                                       *
*                Copyright (C)  2018                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <ncarg/ncargC.h>

extern void NGCALLF(dnggcog,DNGGCOG)(double*,double*,double*,double*,double*,int*);

void c_dnggcog
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
    NGCALLF(dnggcog,DNGGCOG)(&clat,&clon,&crad,alat,alon,&npts);
}
