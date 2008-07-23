/*
 *	$Id: c_fl2int.c,v 1.5 2008-07-23 16:17:01 haley Exp $
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

extern void NGCALLF(fl2int,FL2INT)(float*,float*,int*,int*);

void c_fl2int
#ifdef NeedFuncProto
(
    float px,
    float py,
    int *ix,
    int *iy
)
#else
(px,py,ix,iy)
    float px;
    float py;
    int *ix;
    int *iy;
#endif
{
    NGCALLF(fl2int,FL2INT)(&px,&py,ix,iy);
}
