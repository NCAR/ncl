/*
 *	$Id: c_argeti.c,v 1.1 1997-04-11 17:40:20 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_argeti
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
        fprintf( stderr, "c_argeti:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(ipn);
    ipn2 = NGCstrToFstr(ipn,len);
    NGCALLF(argeti,ARGETI)(ipn2,ivl,len);
}
