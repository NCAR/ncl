/*
 *	$Id: chk_ret_str.c,v 1.1 1997-04-11 17:40:54 haley Exp $
 */
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
