/*
 *	$Id: c_wmsetc.c,v 1.1 1997-04-11 17:45:30 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_wmsetc
#ifdef NeedFuncProto
(
    char *cnp,
    char *cvp
)
#else
( cnp, cvp )
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
        fprintf( stderr, "c_wmsetc:  illegal parameter name (NULL)\n" );
        return;
    }
    len1 = NGSTRLEN(cnp);
    len2 = NGSTRLEN(cvp);
    cnp2 = NGCstrToFstr(cnp,len1);
    cvp2 = NGCstrToFstr(cvp,len2);
    NGCALLF(wmsetc,WMSETC)(cnp2,cvp2,len1,len2);
}


