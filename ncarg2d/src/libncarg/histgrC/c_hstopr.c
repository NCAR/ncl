/*
 *	$Id: c_hstopr.c,v 1.1 1997-04-11 17:43:17 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_hstopr
#ifdef NeedFuncProto
(
    char *iopt,
    float *array,
    int isize
)
#else
(iopt,array,isize)
    char *iopt;
    float *array;
    int isize;
#endif
{
    NGstring iopt2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !iopt ) {
        fprintf( stderr, "c_hstopr:  illegal parameter name (NULL)\n" );
        return;
    }
    len = NGSTRLEN(iopt);
    iopt2 = NGCstrToFstr(iopt,len);
    NGCALLF(hstopr,HSTOPR)(iopt2,array,&isize,len);
}
