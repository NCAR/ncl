/*
 *	$Id: c_dpsmth.c,v 1.5 2008-07-23 16:16:46 haley Exp $
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

extern void NGCALLF(dpsmth,DPSMTH)(float*,float*,int*);

void c_dpsmth
#ifdef NeedFuncProto
(
    float xcpf,
    float ycpf,
    int ifvl
)
#else
(xcpf,ycpf,ifvl)
    float xcpf;
    float ycpf;
    int ifvl;
#endif
{
    NGCALLF(dpsmth,DPSMTH)(&xcpf,&ycpf,&ifvl);
}
