/*
 *	$Id: c_isseti.c,v 1.1 1997-04-11 17:43:23 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_isseti
#ifdef NeedFuncProto
(
    char *ipn,
    int ivl
)
#else
(ipn,ivl)
    char *ipn;
    int ivl;
#endif
{
    NGstring ipn2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !ipn ) {
        fprintf( stderr, "c_isseti: illegal parameter name (NULL)\n" );
        return;
    }
    len = NGSTRLEN(ipn);
    ipn2 = NGCstrToFstr(ipn,len);
    NGCALLF(isseti,ISSETI)(ipn2,&ivl,len);
}
