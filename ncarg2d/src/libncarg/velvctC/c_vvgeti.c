/*
 *	$Id: c_vvgeti.c,v 1.1 1997-04-11 17:45:17 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_vvgeti
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
         fprintf( stderr, "c_vvgeti:  illegal parameter name (NULL)\n" );
         return;
    }
    len = NGSTRLEN(whch);
    whch2 = NGCstrToFstr(whch,len);
    NGCALLF(vvgeti,VVGETI)(whch2,ival,len);
}
