/*
 *	$Id: c_wmgetc.c,v 1.1 1997-04-11 17:45:25 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_wmgetc
#ifdef NeedFuncProto
(
    char *cnp,
    char *cvp,
    int len
)
#else
(cnp,cvp,len)
    char *cnp;
    char *cvp;
    int len;
#endif
{
    int i, len1;
    char error_msg[256];
    NGstring cnp2;
    NGstring cvp2;
/* 
 *  Make sure return string is valid
 */
    if( chk_ret_str( cvp, len, error_msg ) ) {
        fprintf( stderr, "c_wmgetc:  %s\n", error_msg );
        return;
    }
/*
 * Make sure parameter name is not NULL
 */
    if( !cnp ) {
        fprintf( stderr, "c_wmgetc:  illegal parameter name (NULL)\n" );
        return;
    }

    len1 = NGSTRLEN(cnp);
    cvp2 = NGCstrToFstr(cvp,len);
    cnp2 = NGCstrToFstr(cnp,len1);
    NGCALLF(wmgetc,WMGETC)(cnp2,cvp2,len1,len);

    cvp = NGFstrToCstr(cvp2);
    cvp[len] = '\0';
    for( i = len-1; i >= 0; i-- ) {
        if( cvp[i] != ' ' && cvp[i] != '\0' ) {
            cvp[i+1] = '\0';
            break;
        }
    }
}
