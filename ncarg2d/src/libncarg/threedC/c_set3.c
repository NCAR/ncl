/*
 *	$Id: c_set3.c,v 1.5 2008-07-23 16:17:07 haley Exp $
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

extern void NGCALLF(set3,SET3)(float*,float*,float*,float*,float*,float*,
                               float*,float*,float*,float*,float*);

void c_set3 
#ifdef NeedFuncProto
(
    float xa,
    float xb,
    float ya,
    float yb,
    float ulo,
    float uhi,
    float vlo,
    float vhi,
    float wlo,
    float whi,
    float eye[3]
)
#else
 (xa,xb,ya,yb,ulo,uhi,vlo,vhi,wlo,whi,eye)
    float xa;
    float xb;
    float ya;
    float yb;
    float ulo;
    float uhi;
    float vlo;
    float vhi;
    float wlo;
    float whi;
    float eye[3];
#endif
{
    NGCALLF(set3,SET3)(&xa,&xb,&ya,&yb,&ulo,&uhi,&vlo,&vhi,&wlo,&whi,eye);
}
