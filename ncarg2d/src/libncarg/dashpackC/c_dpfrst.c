/*
 *	$Id: c_dpfrst.c,v 1.5 2008-07-23 16:16:45 haley Exp $
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

extern void NGCALLF(dpfrst,DPFRST)(float*,float*);

void c_dpfrst
#ifdef NeedFuncProto
(
    float xcpu,
    float ycpu
)
#else
(xcpu,ycpu)
    float xcpu;
    float ycpu;
#endif
{
    NGCALLF(dpfrst,DPFRST)(&xcpu,&ycpu);
}
