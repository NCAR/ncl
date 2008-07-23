/*
 *      $Id: c_mdqtrn.c,v 1.2 2008-07-23 16:16:51 haley Exp $
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

extern void NGCALLF(mdqtrn,MDQTRN)(double*,double*,double*,double*);

void c_mdqtrn
#ifdef NeedFuncProto
(
    double rlat,
    double rlon,
    double *u,
    double *v
)
#else
(rlat,rlon,u,v)
    double rlat;
    double rlon;
    double *u;
    double *v;
#endif
{
    NGCALLF(mdqtrn,MDQTRN)(&rlat,&rlon,u,v);
}
