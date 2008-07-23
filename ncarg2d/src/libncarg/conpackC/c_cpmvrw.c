/*
 *	$Id: c_cpmvrw.c,v 1.5 2008-07-23 16:16:43 haley Exp $
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

extern void NGCALLF(cpmvrw,CPMVRW)(float*,float*,int*);

void c_cpmvrw
#ifdef NeedFuncProto
(
    float *rwko,
    float *rwrk,
    int lwkn
)
#else
(rwko,rwrk,lwkn)
    float *rwko;
    float *rwrk;
    int lwkn;
#endif
{
    NGCALLF(cpmvrw,CPMVRW)(rwko,rwrk,&lwkn);
}
