/*
 *	$Id: conv_cray_str.c,v 1.4 2008-07-23 16:16:42 haley Exp $
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

_fcd conv_cray_str
#ifdef NeedFuncProto
(
    char *str
)
#else
( str )
    char *str;
#endif
{
    if( str ) return(_cptofcd(str,strlen(str)));
    else      return(_cptofcd("",0));
}
