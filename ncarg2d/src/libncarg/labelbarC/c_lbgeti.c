/*
 *	$Id: c_lbgeti.c,v 1.1 1997-04-11 17:43:31 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_lbgeti
#ifdef NeedFuncProto
(
    char *whch,
    int *ival
)
#else
(whch,ival)
    char *whch;
    int *ival;
#endif
{
    NGstring whch2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !whch ) {
        fprintf( stderr, "c_lbgeti:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(whch);
    whch2 = NGCstrToFstr(whch,len);
    NGCALLF(lbgeti,LBGETI)(whch2,ival,len);
}
