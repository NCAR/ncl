/*
 *	$Id: c_isgeti.c,v 1.1 1997-04-11 17:43:21 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_isgeti
#ifdef NeedFuncProto
(
    char *ipn,
    int *ivl
)
#else
(ipn,ivl)
    char *ipn;
    int *ivl;
#endif
{
    NGstring ipn2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !ipn ) {
        fprintf( stderr, "c_isgeti: illegal parameter name (NULL)\n" );
        return;
    }
    len = NGSTRLEN(ipn);
    ipn2 = NGCstrToFstr(ipn,len);
    NGCALLF(isgeti,ISGETI)(ipn2,ivl,len);
}
