/*
 *      $Id: c_tdgtrs.c,v 1.6 2008-07-23 16:17:06 haley Exp $
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

extern void NGCALLF(tdgtrs,TDGTRS)(int*,int*,int*,int*,int*,int*,
                                   int*,int*,float*,float*,float*);

void c_tdgtrs
#ifdef NeedFuncProto
(
    int    irst,
    int   *ifc1,
    int   *ifc2,
    int   *ifc3,
    int   *ifc4,
    int   *ilc1,
    int   *ilc2,
    int   *iltd,
    float *ustp,
    float *vstp,
    float *wstp
)
#else
(irst,ifc1,ifc2,ifc3,ifc4,ilc1,ilc2,iltd,ustp,vstp,wstp)
    int    irst;
    int   *ifc1;
    int   *ifc2;
    int   *ifc3;
    int   *ifc4;
    int   *ilc1;
    int   *ilc2;
    int   *iltd;
    float *ustp;
    float *vstp;
    float *wstp;
#endif
{
    NGCALLF(tdgtrs,TDGTRS)(&irst,ifc1,ifc2,ifc3,ifc4,ilc1,
                                 ilc2,iltd,ustp,vstp,wstp);
}
