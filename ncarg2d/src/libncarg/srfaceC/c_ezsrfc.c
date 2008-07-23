/*
 *	$Id: c_ezsrfc.c,v 1.5 2008-07-23 16:17:02 haley Exp $
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

extern void NGCALLF(ezsrfc,EZSRFC)(float*,int*,int*,float*,float*,float*);

void c_ezsrfc
#ifdef NeedFuncProto
(
    float *z,
    int m,
    int n,
    float angh,
    float angv,
    float *work
)
#else
(z,m,n,angh,angv,work)
    float *z;
    int m;
    int n;
    float angh;
    float angv;
    float *work;
#endif
{
    NGCALLF(ezsrfc,EZSRFC)(z,&m,&n,&angh,&angv,work);
}
