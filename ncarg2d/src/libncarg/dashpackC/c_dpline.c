/*
 *	$Id: c_dpline.c,v 1.5 2008-07-23 16:16:46 haley Exp $
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

extern void NGCALLF(dpline,DPLINE)(float*,float*,float*,float*);

void c_dpline
#ifdef NeedFuncProto
(
    float xcp1,
    float ycp1,
    float xcp2,
    float ycp2
)
#else
(xcp1,ycp1,xcp2,ycp2)
    float xcp1;
    float ycp1;
    float xcp2;
    float ycp2;
#endif
{
    NGCALLF(dpline,DPLINE)(&xcp1,&ycp1,&xcp2,&ycp2);
}
