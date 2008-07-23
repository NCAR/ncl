/*
 *      $Id: c_mdutfs.c,v 1.2 2008-07-23 16:16:52 haley Exp $
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

extern void NGCALLF(mdutfs,MDUTFS)(float*,float*,float*,float*);

void c_mdutfs
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
    NGCALLF(mdutfs,MDUTFS)(&rlat,&rlon,u,v);
}
