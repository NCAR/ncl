/*
 *	$Id: c_vect3.c,v 1.5 2008-07-23 16:17:07 haley Exp $
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

extern void NGCALLF(vect3,VECT3)(float*,float*,float*);

void c_vect3
#ifdef NeedFuncProto
(
    float u,
    float v,
    float w
)
#else
(u,v,w)
    float u;
    float v;
    float w;
#endif
{
    NGCALLF(vect3,VECT3)(&u,&v,&w);
}
