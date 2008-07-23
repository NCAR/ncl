/*
 *	$Id: c_tick43.c,v 1.5 2008-07-23 16:17:07 haley Exp $
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

extern void NGCALLF(tick43,TICK43)(int*,int*,int*,int*,int*,int*);

void c_tick43 
#ifdef NeedFuncProto
(
    int magu,
    int minu,
    int magv,
    int minv,
    int magw,
    int minw
)
#else
 (magu,minu,magv,minv,magw,minw)
    int magu;
    int minu;
    int magv;
    int minv;
    int magw;
    int minw;
#endif
{
    NGCALLF(tick43,TICK43)(&magu,&minu,&magv,&minv,&magw,&minw);
}
