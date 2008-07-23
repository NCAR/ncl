/*
 *	$Id: c_ngmftc.c,v 1.5 2008-07-23 16:16:58 haley Exp $
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

extern void NGCALLF(ngmftc,NGMFTC)(int*);

void c_ngmftc
#ifdef NeedFuncProto
(
    int wkid
)
#else
(wkid)
    int wkid;
#endif
{
    NGCALLF(ngmftc,NGMFTC)(&wkid);
}
