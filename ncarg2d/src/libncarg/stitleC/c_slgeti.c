/*
 *	$Id: c_slgeti.c,v 1.1 1997-04-11 17:44:37 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_slgeti
#ifdef NeedFuncProto
(
    char *pa,
    int *ival
)
#else
(pa,ival)
    char *pa;
    int *ival;
#endif
{
    NGstring pa2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !pa ) {
        fprintf( stderr, "c_slgeti:  illegal parameter name (NULL)\n" );
        return;
    }
    len = NGSTRLEN(pa);
    pa2 = NGCstrToFstr(pa,len);
    NGCALLF(slgeti,SLGETI)(pa2,ival,len);
}
