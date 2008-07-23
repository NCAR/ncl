/*
 *	$Id: c_ticks.c,v 1.5 2008-07-23 16:16:55 haley Exp $
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

extern void NGCALLF(ticks,ticks)(int*,int*);


void c_ticks
#ifdef NeedFuncProto
(
    int lmjr,
    int lmnr
)
#else
(lmjr,lmnr)
    int lmjr;
    int lmnr;
#endif
{
    NGCALLF(ticks,TICKS)(&lmjr,&lmnr);
}
