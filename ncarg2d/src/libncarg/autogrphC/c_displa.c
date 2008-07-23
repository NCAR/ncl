/*
 *	$Id: c_displa.c,v 1.5 2008-07-23 16:16:41 haley Exp $
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

extern void NGCALLF(displa,DISPLA)(int*,int*,int*);

void c_displa
#ifdef NeedFuncProto
(
    int lfra,
    int lrow,
    int ltyp
)
#else
(lfra,lrow,ltyp)
    int lfra;
    int lrow;
    int ltyp;
#endif
{
    NGCALLF(displa,DISPLA)(&lfra,&lrow,&ltyp);
}
