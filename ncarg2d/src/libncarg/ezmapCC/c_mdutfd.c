/*
 *      $Id: c_mdutfd.c,v 1.2 2008-07-23 16:16:52 haley Exp $
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

extern void NGCALLF(mdutfd,MDUTFD)(double*,double*,double*,double*);

void c_mdutfd
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
    NGCALLF(mdutfd,MDUTFD)(&rlat,&rlon,u,v);
}
