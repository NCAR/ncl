/*
 *	$Id: c_hstopl.c,v 1.1 1997-04-11 17:43:16 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_hstopl
#ifdef NeedFuncProto
(
    char *iopt
)
#else
(iopt)
    char *iopt;
#endif
{
    NGstring iopt2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !iopt ) {
        fprintf( stderr, "c_hstopl:  illegal parameter name (NULL)\n" );
        return;
    }
    len = NGSTRLEN(iopt);
    iopt2 = NGCstrToFstr(iopt,len);
    NGCALLF(hstopl,HSTOPL)(iopt2,len);
}
