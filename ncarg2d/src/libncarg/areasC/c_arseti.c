/*
 *	$Id: c_arseti.c,v 1.1 1997-04-11 17:40:24 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_arseti
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
        fprintf( stderr, "c_arseti:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(ipn);
	ipn2 = NGCstrToFstr(ipn,len);
    NGCALLF(arseti,ARSETI)(ipn2,&ivl,len);
}
