/*
 *	$Id: c_dpgetc.c,v 1.1 1997-04-11 17:41:42 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_dpgetc
#ifdef NeedFuncProto
(
    char *pnam,
    char *cval,
    int len
)
#else
(pnam,cval,len)
    char *pnam;
    char *cval;
    int len;
#endif
{
    int i, len1;
    char error_msg[256];
    NGstring pnam2;
    NGstring cval2;
/* 
 *  Make sure return string is valid
 */
    if( chk_ret_str( cval, len, error_msg ) ) {
        fprintf( stderr, "c_dpgetc:  %s\n", error_msg );
        return;
    }
/*
 * Make sure parameter name is not NULL
 */
    if( !pnam ) {
        fprintf( stderr, "c_dpgetc:  illegal parameter name (NULL)\n" );
        return;
    }

    len1 = NGSTRLEN(pnam);
    cval2 = NGCstrToFstr(cval,len);
    pnam2 = NGCstrToFstr(pnam,len1);
    NGCALLF(dpgetc,DPGETC)(pnam2,cval2,len1,len);

    cval = NGFstrToCstr(cval2);
    cval[len] = '\0';
    for( i = len-1; i >= 0; i-- ) {
        if( cval[i] != ' ' && cval[i] != '\0' ) {
            cval[i+1] = '\0';
            break;
        }
    }
}
