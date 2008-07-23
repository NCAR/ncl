/*
 *      $Id: c_tdclrs.c,v 1.5 2008-07-23 16:17:05 haley Exp $
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

extern void NGCALLF(tdclrs,TDCLRS)(int*,int*,float*,float*,int*,int*,int*);

void c_tdclrs
#ifdef NeedFuncProto
(
    int   iwid,
    int   ibow,
    float shde,
    float shdr,
    int   iofc,
    int   iolc,
    int   ilmt
)
#else
(iwid,ibow,shde,shdr,iofc,iolc,ilmt)
    int   iwid;
    int   ibow;
    float shde;
    float shdr;
    int   iofc;
    int   iolc;
    int   ilmt;
#endif
{
    NGCALLF(tdclrs,TDCLRS)(&iwid,&ibow,&shde,&shdr,&iofc,&iolc,&ilmt);
}
