/*
 *	$Id: c_msceez.c,v 1.5 2008-07-23 16:17:04 haley Exp $
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

extern void NGCALLF(msceez,MSCEEZ)(float*,float*,float*,float*,float*,
                                   float*,int*);

void c_msceez 
#ifdef NeedFuncProto
(
    float del1,
    float del2,
    float sigma,
    float *c1,
    float *c2,
    float *c3,
    int n
)
#else
(del1,del2,sigma,c1,c2,c3,n)
    float del1;
    float del2;
    float sigma;
    float *c1;
    float *c2;
    float *c3;
    int n;
#endif
{
    NGCALLF(msceez,MSCEEZ)(&del1,&del2,&sigma,c1,c2,c3,&n);
}
