/*
 *	$Id: c_ngckop.c,v 1.5 2008-07-23 16:16:57 haley Exp $
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

extern int NGCALLF(ngckop,NGCKOP)(int*);

int c_ngckop
#ifdef NeedFuncProto
(
    int wkid
)
#else
(wkid)
    int wkid;
#endif
{
    return(NGCALLF(ngckop,NGCKOP)(&wkid));
}
