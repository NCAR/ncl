/*
 *	$Id: c_isosrf.c,v 1.1 1997-04-11 17:43:22 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_isosrf 
#ifdef NeedFuncProto
(
    float *t,
    int lu,
    int mu,
    int lv,
    int mv,
    int mw,
    float eye[3],
    int muvwp2,
    float *slab,
    float tiso,
    int iflag
)
#else
 (t,lu,mu,lv,mv,mw,eye,muvwp2,slab,tiso,iflag)
    float *t;
    int lu;
    int mu;
    int lv;
    int mv;
    int mw;
    float eye[3];
    int muvwp2;
    float *slab;
    float tiso;
    int iflag;
#endif
{
    NGCALLF(isosrf,ISOSRF)(t,&lu,&mu,&lv,&mv,&mw,eye,&muvwp2,slab,&tiso,&iflag);
}
