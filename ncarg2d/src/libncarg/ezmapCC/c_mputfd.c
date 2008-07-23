/*
 *      $Id: c_mputfd.c,v 1.2 2008-07-23 16:16:54 haley Exp $
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

extern void NGCALLF(mputfd,MPUTFD)(double*,double*,double*,double*);

void c_mputfd
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
    NGCALLF(mputfd,MPUTFD)(&rlat,&rlon,u,v);
}
