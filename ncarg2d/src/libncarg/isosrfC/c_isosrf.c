/*
 *	$Id: c_isosrf.c,v 1.5 2008-07-23 16:16:56 haley Exp $
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

extern void NGCALLF(isosrf,ISOSRF)(float*,int*,int*,int*,int*,int*,float*,
                                   int*,float*,float*,int*);

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
    NGCALLF(isosrf,ISOSRF)(t,&lu,&mu,&lv,&mv,&mw,eye,&muvwp2,slab,&tiso,
                           &iflag);
}
