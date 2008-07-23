/*
 *	$Id: c_retsr.c,v 1.5 2008-07-23 16:17:05 haley Exp $
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

extern void NGCALLF(retsr,RETSR)(int*);


void c_retsr
#ifdef NeedFuncProto
(
    int irold
)
#else
(irold)
    int irold;
#endif
{
    NGCALLF(retsr,RETSR)(&irold);
}
