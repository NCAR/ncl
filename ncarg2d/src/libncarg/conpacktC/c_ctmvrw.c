/*
 *      $Id: c_ctmvrw.c,v 1.2 2008-07-23 16:16:44 haley Exp $
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

extern void NGCALLF(ctmvrw,CTMVRW)(float*,float*,int*);

void c_ctmvrw
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
    NGCALLF(ctmvrw,CTMVRW)(rwko,rwrk,&lwkn);
}
