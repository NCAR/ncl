/*
 *	$Id: c_seti.c,v 1.5 2008-07-23 16:17:02 haley Exp $
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

extern void NGCALLF(seti,SETI)(int*,int*);

void c_seti
#ifdef NeedFuncProto
(
    int ix,
    int iy 
)
#else
(ix,iy)
    int ix;
    int iy;
#endif
{
    NGCALLF(seti,SETI)(&ix,&iy);
}
