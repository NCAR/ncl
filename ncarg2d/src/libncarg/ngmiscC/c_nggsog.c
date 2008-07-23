/*
 *	$Id: c_nggsog.c,v 1.5 2008-07-23 16:16:58 haley Exp $
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

extern void NGCALLF(nggsog,NGGSOG)(float*,float*,float*,float*,float*);

void c_nggsog
#ifdef NeedFuncProto
(
    float slat,
    float slon,
    float srad,
    float *alat,
    float *alon
)
#else
(slat,slon,srad,alat,alon)
    float slat;
    float slon;
    float srad;
    float *alat;
    float *alon;
#endif
{
    NGCALLF(nggsog,NGGSOG)(&slat,&slon,&srad,alat,alon);
}
