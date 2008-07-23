/*
 *	$Id: c_wmw2ny.c,v 1.5 2008-07-23 16:17:09 haley Exp $
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

extern void NGCALLF(wmw2ny,WMW2NY)(int*,float*,float*);

void c_wmw2ny
#ifdef NeedFuncProto
(
    int npt,
    float *p,
    float *q
)
#else
(npt,p,q)
    int npt;
    float *p;
    float *q;
#endif
{
    NGCALLF(wmw2ny,WMW2NY)(&npt,p,q);
}
