/*
 *	$Id: c_nggcog.c,v 1.5 2008-07-23 16:16:57 haley Exp $
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

extern void NGCALLF(nggcog,NGGCOG)(float*,float*,float*,float*,float*,int*);

void c_nggcog
#ifdef NeedFuncProto
(
    float clat,
    float clon,
    float crad,
    float *alat,
    float *alon,
    int npts
)
#else
(clat,clon,crad,alat,alon,npts)
    float clat;
    float clon;
    float crad;
    float *alat;
    float *alon;
    int npts;
#endif
{
    NGCALLF(nggcog,NGGCOG)(&clat,&clon,&crad,alat,alon,&npts);
}
