/*
 *	$Id: c_tick4.c,v 1.5 2008-07-23 16:16:55 haley Exp $
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

extern void NGCALLF(tick4,TICK4)(int*,int*,int*,int*);

void c_tick4
#ifdef NeedFuncProto
(
    int lmjx,
    int lmnx,
    int lmjy,
    int lmny
)
#else
(lmjx,lmnx,lmjy,lmny)
    int lmjx;
    int lmnx;
    int lmjy;
    int lmny;
#endif
{
    NGCALLF(tick4,TICK4)(&lmjx,&lmnx,&lmjy,&lmny);
}
