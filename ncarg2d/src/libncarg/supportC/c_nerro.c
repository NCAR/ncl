/*
 *	$Id: c_nerro.c,v 1.1 1997-04-11 17:45:01 haley Exp $
 */
#include <ncarg/ncargC.h>

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
