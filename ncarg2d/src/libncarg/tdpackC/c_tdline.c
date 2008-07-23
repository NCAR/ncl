/*
 *      $Id: c_tdline.c,v 1.5 2008-07-23 16:17:06 haley Exp $
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

extern void NGCALLF(tdline,TDLINE)(float*,float*,float*,float*,float*,float*);

void c_tdline
#ifdef NeedFuncProto
(
    float ucp1,
    float vcp1,
    float wcp1,
    float ucp2,
    float vcp2,
    float wcp2
)
#else
(ucp1,vcp1,wcp1,ucp2,vcp2,wcp2)
    float ucp1;
    float vcp1;
    float wcp1;
    float ucp2;
    float vcp2;
    float wcp2;
#endif
{
    NGCALLF(tdline,TDLINE)(&ucp1,&vcp1,&wcp1,&ucp2,&vcp2,&wcp2);
}
