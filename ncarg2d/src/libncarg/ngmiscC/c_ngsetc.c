/*
 *	$Id: c_ngsetc.c,v 1.1 1997-04-11 17:43:42 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ngsetc
#ifdef NeedFuncProto
(
    char *pnam,
    char *cvp
)
#else
( pnam, cvp )
    char *pnam;
    char *cvp;
#endif
{
    NGstring pnam2;
    NGstring cvp2;
    int len1, len2;
/*
 * Make sure parameter name is not NULL
 */
    if( !pnam ) { 
        fprintf( stderr, "c_ngsetc:  illegal parameter name (NULL)\n" );
        return;
    }
    len1 = NGSTRLEN(pnam);
    len2 = NGSTRLEN(cvp);
    pnam2 = NGCstrToFstr(pnam,len1);
    cvp2 = NGCstrToFstr(cvp,len2);
    NGCALLF(ngsetc,NGSETC)(pnam2,cvp2,len1,len2);
}


