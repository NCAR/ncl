/*
 *	$Id: c_issetr.c,v 1.1 1997-04-11 17:43:24 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_issetr
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
        fprintf( stderr, "c_issetr: illegal parameter name (NULL)\n" );
        return;
    }

    rvl2 = rvl;
    len = NGSTRLEN(ipn);
    ipn2 = NGCstrToFstr(ipn,len);
    NGCALLF(issetr,ISSETR)(ipn2,&rvl2,len);
}
