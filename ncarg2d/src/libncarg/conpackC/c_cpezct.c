/*
 *	$Id: c_cpezct.c,v 1.5 2008-07-23 16:16:42 haley Exp $
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

extern void NGCALLF(cpezct,CPEZCT)(float*,int*,int*);

void c_cpezct
#ifdef NeedFuncProto
(
    float *zdat,
    int mzdt,
    int nzdt
)
#else
(zdat,mzdt,nzdt)
    float *zdat;
    int mzdt;
    int nzdt;
#endif
{
    NGCALLF(cpezct,CPEZCT)(zdat,&mzdt,&nzdt);
}
