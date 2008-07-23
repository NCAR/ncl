/*
 *      $Id: c_mputfs.c,v 1.2 2008-07-23 16:16:54 haley Exp $
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

extern void NGCALLF(mputfs,MPUTFS)(float*,float*,float*,float*);

void c_mputfs
#ifdef NeedFuncProto
(
    float rlat,
    float rlon,
    float *u,
    float *v
)
#else
(rlat,rlon,u,v)
    float rlat;
    float rlon;
    float *u;
    float *v;
#endif
{
    NGCALLF(mputfs,MPUTFS)(&rlat,&rlon,u,v);
}
