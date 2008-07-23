/*
 *	$Id: c_lined.c,v 1.5 2008-07-23 16:16:45 haley Exp $
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

extern void NGCALLF(lined,LINED)(float*,float*,float*,float*);

void c_lined
#ifdef NeedFuncProto
(
    float xa,
    float ya,
    float xb,
    float yb
)
#else
(xa,ya,xb,yb)
    float xa;
    float ya;
    float xb;
    float yb;
#endif
{
    NGCALLF(lined,LINED)(&xa,&ya,&xb,&yb);
}
