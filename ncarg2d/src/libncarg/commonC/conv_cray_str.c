/*
 *	$Id: conv_cray_str.c,v 1.1 1997-04-11 17:40:54 haley Exp $
 */
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
