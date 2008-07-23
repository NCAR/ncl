/*
 *	$Id: chk_ret_str.c,v 1.4 2008-07-23 16:16:42 haley Exp $
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

int chk_ret_str
#ifdef NeedFuncProto
(
    char *str,
    int len,
    char *error_msg
)
#else
( str, len, error_msg )
    char *str;
    int len;
    char *error_msg;
#endif
{
    if( !str ) {
        sprintf( error_msg, "illegal return string (NULL)\n");
        return(1);
    }
    if( len <= 0 ) {
        sprintf( error_msg, "illegal return string length = %d", len );
        return(1);
    }
    return(0);
}
