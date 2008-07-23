/*
 *	$Id: c_vectd.c,v 1.5 2008-07-23 16:16:45 haley Exp $
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

extern void NGCALLF(vectd,VECTD)(float*,float*);

void c_vectd
#ifdef NeedFuncProto
(
    float x,
    float y
)
#else
(x,y)
    float x;
    float y;
#endif
{
    NGCALLF(vectd,VECTD)(&x,&y);
}
