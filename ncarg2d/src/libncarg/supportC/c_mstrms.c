/*
 *	$Id: c_mstrms.c,v 1.5 2008-07-23 16:17:05 haley Exp $
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

extern void NGCALLF(mstrms,MSTRMS)(float*,float*,float*,float*);


void c_mstrms 
#ifdef NeedFuncProto
(
    float *diag,
    float *sdiag,
    float sigma,
    float del
)
#else
(diag,sdiag,sigma,del)
    float *diag;
    float *sdiag;
    float sigma;
    float del;
#endif
{
    NGCALLF(mstrms,MSTRMS)(diag,sdiag,&sigma,&del);
}
