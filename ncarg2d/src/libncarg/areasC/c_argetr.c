/*
 *	$Id: c_argetr.c,v 1.1 1997-04-11 17:40:21 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_argetr
#ifdef NeedFuncProto
(
    char *ipn,
    float *rvl
)
#else
(ipn,rvl)
    char *ipn;
    float *rvl;
#endif
{
	NGstring ipn2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !ipn ) { 
        fprintf( stderr, "c_argetr:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(ipn);
	ipn2 = NGCstrToFstr(ipn,len);
    NGCALLF(argetr,ARGETR)(ipn2,rvl,len);
}
