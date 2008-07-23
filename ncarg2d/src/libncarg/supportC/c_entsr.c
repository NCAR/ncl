/*
 *	$Id: c_entsr.c,v 1.5 2008-07-23 16:17:04 haley Exp $
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

extern void NGCALLF(entsr,ENTSR)(int*,int*);


void c_entsr
#ifdef NeedFuncProto
(
    int *irold,
    int irnew
)
#else
(irold,irnew)
    int *irold;
    int irnew;
#endif
{
    NGCALLF(entsr,ENTSR)( irold, &irnew);
}
