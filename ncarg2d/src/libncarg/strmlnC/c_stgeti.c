/*
 *	$Id: c_stgeti.c,v 1.1 1997-04-11 17:44:48 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_stgeti
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
        fprintf( stderr, "c_stgeti:  illegal parameter name (NULL)\n" );
        return;
    }
    len = NGSTRLEN(whch);
    whch2 = NGCstrToFstr(whch,len);
    NGCALLF(stgeti,STGETI)(whch2,ival,len);
}
