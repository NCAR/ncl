/*
 *	$Id: c_line3.c,v 1.5 2008-07-23 16:17:07 haley Exp $
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

extern void NGCALLF(line3,LINE3)(float*,float*,float*,float*,float*,float*);

void c_line3 
#ifdef NeedFuncProto
(
    float ua,
    float va,
    float wa,
    float ub,
    float vb,
    float wb
)
#else
 (ua,va,wa,ub,vb,wb)
    float ua;
    float va;
    float wa;
    float ub;
    float vb;
    float wb;
#endif
{
    NGCALLF(line3,LINE3)(&ua,&va,&wa,&ub,&vb,&wb);
}
