/*
 *      $Id: c_tdpara.c,v 1.6 2008-07-23 16:17:06 haley Exp $
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

extern void NGCALLF(tdpara,TDPARA)(float*,float*,float*,float*,float*,float*,
                                                        float*,float*,float*);

void c_tdpara
#ifdef NeedFuncProto
(
    float ua00,
    float va00,
    float wa00,
    float uv10,
    float vv10,
    float wv10,
    float uv01,
    float vv01,
    float wv01
)
#else
(ua00,va00,wa00,uv10,vv10,wv10,uv01,vv01,wv01)
    float ua00;
    float va00;
    float wa00;
    float uv10;
    float vv10;
    float wv10;
    float uv01;
    float vv01;
    float wv01;
#endif
{
    NGCALLF(tdpara,TDPARA)(&ua00,&va00,&wa00,&uv10,&vv10,&wv10,
                                             &uv01,&vv01,&wv01);
}
