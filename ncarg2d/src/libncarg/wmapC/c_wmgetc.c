/*
 *	$Id: c_wmgetc.c,v 1.6 2008-07-23 16:17:08 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <ncarg/ncargC.h>

extern void NGCALLF(wmgetc,WMGETC)(NGstring,NGstring,int,int);

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
    NGCALLF(wmgetc,WMGETC)(cnp2,cvp2,len1,len-1);

    cvp = NGFstrToCstr(cvp2);
    cvp[len-1] = '\0';
    for( i = len-2; i >= 0; i-- ) {
        if( cvp[i] != ' ' && cvp[i] != '\0' ) {
            cvp[i+1] = '\0';
            break;
        }
    }
}
