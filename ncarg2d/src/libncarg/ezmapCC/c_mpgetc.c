/*
 *	$Id: c_mpgetc.c,v 1.2 2008-07-23 16:16:52 haley Exp $
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

extern void NGCALLF(mpgetc,MPGETC)(NGstring,NGstring,int,int);

void c_mpgetc
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
    int i, len1;
    char error_msg[256];
    NGstring whch2;
    NGstring cval2;
/* 
 *  Make sure return string is valid
 */
    if( chk_ret_str( cval, len, error_msg ) ) {
        fprintf( stderr, "c_mpgetc:  %s\n", error_msg );
        return;
    }
/*
 * Make sure parameter name is not NULL
 */
    if( !whch ) { 
        fprintf( stderr, "c_mpgetc:  illegal parameter string (NULL)\n" );
        return;
    }
    len1 = NGSTRLEN(whch);
    whch2 = NGCstrToFstr(whch,len1);
    cval2 = NGCstrToFstr(cval,len);
    NGCALLF(mpgetc,MPGETC)(whch2,cval2,len1,len-1);

    cval = NGFstrToCstr(cval2);
    cval[len-1] = '\0';
    for( i = len-2; i >= 0; i-- ) {
        if( cval[i] != ' ' && cval[i] != '\0' ) {
            cval[i+1] = '\0';
            break;
        }
    }
}
