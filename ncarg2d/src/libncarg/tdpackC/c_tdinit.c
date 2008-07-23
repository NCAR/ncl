/*
 *      $Id: c_tdinit.c,v 1.6 2008-07-23 16:17:06 haley Exp $
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

extern void NGCALLF(tdinit,TDINIT)(float*,float*,float*,float*,float*,float*,
                                   float*,float*,float*,float*);

void c_tdinit
#ifdef NeedFuncProto
(
    float umid,
    float vmid,
    float wmid,
    float uori,
    float vori,
    float wori,
    float uthi,
    float vthi,
    float wthi,
    float otep
)
#else
(umid,vmid,wmid,uori,vori,wori,uthi,vthi,wthi,otep)
    float umid;
    float vmid;
    float wmid;
    float uori;
    float vori;
    float wori;
    float uthi;
    float vthi;
    float wthi;
    float otep;
#endif
{
    NGCALLF(tdinit,TDINIT)(&umid,&vmid,&wmid,&uori,&vori,&wori,
                                       &uthi,&vthi,&wthi,&otep);
}
