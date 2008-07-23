/*
 *	$Id: c_nerro.c,v 1.5 2008-07-23 16:17:05 haley Exp $
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

extern int NGCALLF(nerro,NERRO)(int*);

int c_nerro
#ifdef NeedFuncProto
(
    int *nerr
)
#else
(nerr)
    int *nerr;
#endif
{
    return(NGCALLF(nerro,NERRO)(nerr));
}
