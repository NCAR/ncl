/*
 *	$Id: c_arsetr.c,v 1.1 1997-04-11 17:40:25 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_arsetr
#ifdef NeedFuncProto
(
    char *ipn,
    float rvl
)
#else
(ipn,rvl)
    char *ipn;
    float rvl;
#endif
{
    float rvl2;
	NGstring ipn2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !ipn ) { 
        fprintf( stderr, "c_arsetr:  illegal parameter string (NULL)\n" );
        return;
    }

    rvl2 = rvl;

    len = NGSTRLEN(ipn);
	ipn2 = NGCstrToFstr(ipn,len);
    NGCALLF(arsetr,ARSETR)(ipn2,&rvl2,len);
}
