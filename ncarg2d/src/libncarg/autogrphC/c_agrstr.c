/*
 *	$Id: c_agrstr.c,v 1.5 2008-07-23 16:16:40 haley Exp $
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

extern void NGCALLF(agrstr,AGRSTR)(int*);

void c_agrstr
#ifdef NeedFuncProto
(
    int ifno
)
#else
(ifno)
    int ifno;
#endif
{
    NGCALLF(agrstr,AGRSTR)(&ifno);
}
