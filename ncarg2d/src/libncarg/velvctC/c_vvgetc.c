/*
 *	$Id: c_vvgetc.c,v 1.2 1997-12-19 22:22:32 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_vvgetc
#ifdef NeedFuncProto
(
    char *whch,
    char *cval,
    int len
)
#else
(whch,cval,len)
    char *whch;
    char *cval;
    int len;
#endif
{
    int i;
    char error_msg[256];

    NGstring whch2;
    NGstring cval2;
    int len1;
/* 
 *  Make sure return string is valid
 */
    if( chk_ret_str( cval, len, error_msg ) ) {
        fprintf( stderr, "c_vvgetc:  %s\n", error_msg );
        return;
    }
/* 
 * Make sure parameter name is not NULL
 */
    if( !whch ) {
         fprintf( stderr, "c_vvgetc:  illegal parameter name (NULL)\n" );
         return;
    }
    len1 = NGSTRLEN(whch);
    whch2 = NGCstrToFstr(whch,len1);
    cval2 = NGCstrToFstr(cval,len);
    NGCALLF(vvgetc,VVGETC)(whch2,cval2,len1,len-1);
    cval = NGFstrToCstr(cval2);
    cval[len-1] = '\0';
    for( i = len-2; i >= 0; i-- ) {
        if( cval[i] != ' ' && cval[i] != '\0' ) {
            cval[i+1] = '\0';
            break;
        }
    }
}
