/*
 *	$Id: c_pcgeti.c,v 1.1 1997-04-11 17:43:49 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_pcgeti
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
        fprintf( stderr, "c_pcgeti:  illegal parameter name (NULL)\n" );
        return;
    }
    len = NGSTRLEN(whch);
    whch2 = NGCstrToFstr(whch,len);
    NGCALLF(pcgeti,PCGETI)(whch2,ival,len);
}
