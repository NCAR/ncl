/*
 *	$Id: c_ngpict.c,v 1.5 2008-07-23 16:16:58 haley Exp $
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

extern void NGCALLF(ngpict,NGPICT)(int*,int*);

void c_ngpict
#ifdef NeedFuncProto
(
    int wkid,
    int action
)
#else
(wkid,action)
    int wkid;
    int action;
#endif
{
    NGCALLF(ngpict,NGPICT)(&wkid,&action);
}
