/*
 *	$Id: c_sfsetc.c,v 1.1 1997-04-11 17:44:09 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_sfsetc
#ifdef NeedFuncProto
(
    char *cnp,
    char *cvp
)
#else
(cnp,cvp)
    char *cnp;
    char *cvp;
#endif
{
    NGstring cnp2;
    NGstring cvp2;
    int len1, len2;
/*
 * Make sure parameter name is not NULL
 */
    if( !cnp ) {
        fprintf( stderr, "c_sfsetc:  illegal parameter name (NULL)\n");
        return;
    }
    len1 = NGSTRLEN(cnp);
    len2 = NGSTRLEN(cvp);
    cnp2 = NGCstrToFstr(cnp,len1);
    cvp2 = NGCstrToFstr(cvp,len2);
    NGCALLF(sfsetc,SFSETC)(cnp2,cvp2,len1,len2);
}
