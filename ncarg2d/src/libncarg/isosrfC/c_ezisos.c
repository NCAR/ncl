/*
 *	$Id: c_ezisos.c,v 1.1 1997-04-11 17:43:20 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ezisos 
#ifdef NeedFuncProto
(
    float *t,
    int mu,
    int mv,
    int mw,
    float eye[3],
    float *slab,
    float tiso
)
#else
 (t,mu,mv,mw,eye,slab,tiso)
    float *t;
    int mu;
    int mv;
    int mw;
    float eye[3];
    float *slab;
    float tiso;
#endif
{
    NGCALLF(ezisos,EZISOS)(t,&mu,&mv,&mw,eye,slab,&tiso);
}
