/*
 *	$Id: c_set3.c,v 1.1 1997-04-11 17:45:11 haley Exp $
 */
#include <ncarg/ncargC.h>

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
    float xa2, xb2, ya2, yb2, ulo2, uhi2, vlo2, vhi2, wlo2, whi2;
    xa2 = xa;
    xb2 = xb;
    ya2 = ya;
    yb2 = yb;
    ulo2 = ulo;
    uhi2 = uhi;
    vlo2 = vlo;
    vhi2 = vhi;
    wlo2 = wlo;
    whi2 = whi;
    NGCALLF(set3,SET3)(&xa2,&xb2,&ya2,&yb2,&ulo2,&uhi2,&vlo2,&vhi2,&wlo2,&whi2,eye);
}
