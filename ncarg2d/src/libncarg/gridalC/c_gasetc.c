/*
 *	$Id: c_gasetc.c,v 1.1 1997-04-11 17:42:48 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_gasetc
#ifdef NeedFuncProto
(
    char *pnam,
    char *cval
)
#else
(pnam,cval)
    char *pnam;
    char *cval;
#endif
{
    NGstring pnam2;
    NGstring cval2;
    int len1, len2;
/*
 *  Make sure parameter name is not NULL
 */
    if( !pnam ) {
        fprintf( stderr, "c_gasetc:  illegal parameter string (NULL)\n");
        return;
    }
/*
 *  Make sure return string is not NULL
 */
    if( !cval ) {
        fprintf( stderr, "c_gasetc:  illegal parameter string (NULL)\n");
        return;
    }
    len1 = NGSTRLEN(pnam);
    len2 = NGSTRLEN(cval);
    pnam2 = NGCstrToFstr(pnam,len1);
    cval2 = NGCstrToFstr(cval,len2);
    NGCALLF(gasetc,GASETC)(pnam2,cval2,len1,len2);
}
