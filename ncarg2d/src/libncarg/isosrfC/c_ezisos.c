/*
 *	$Id: c_ezisos.c,v 1.5 2008-07-23 16:16:56 haley Exp $
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

extern void NGCALLF(ezisos,EZISOS)(float*,int*,int*,int*,float*,float*,
                                   float*);

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
