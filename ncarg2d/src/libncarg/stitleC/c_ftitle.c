/*
 *	$Id: c_ftitle.c,v 1.5 2008-07-23 16:17:03 haley Exp $
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

extern void NGCALLF(ftitle,FTITLE)(int*);

void c_ftitle
#ifdef NeedFuncProto
(
    int mtst
)
#else
(mtst)
    int mtst;
#endif
{
    NGCALLF(ftitle,FTITLE)(&mtst);
}
