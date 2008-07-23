/*
 *	$Id: c_aggetc.c,v 1.6 2008-07-23 16:16:40 haley Exp $
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

extern void NGCALLF(aggetc,AGGETC)(NGstring,NGstring,int,int);

void c_aggetc
#ifdef NeedFuncProto
(
    char *tpid,
    char *cusr,
    int len
)
#else
(tpid,cusr,len)
    char *tpid;
    char *cusr;
    int len;
#endif
{
    int i, len1;
    char error_msg[256];

    NGstring tpid2;
    NGstring cusr2;
/* 
 *  Make sure return string is valid
 */
    if( chk_ret_str( cusr, len, error_msg ) ) {
        fprintf( stderr, "c_aggetc:  %s\n", error_msg );
        return;
    }
/*
 * Make sure parameter name is not NULL
 */
    if( !tpid ) { 
        fprintf( stderr, "c_aggetc:  illegal parameter string (NULL)\n" );
        return;
    }

    len1 = NGSTRLEN(tpid);
    cusr2 = NGCstrToFstr(cusr,len);
    tpid2 = NGCstrToFstr(tpid,len1);
    NGCALLF(aggetc,AGGETC)(tpid2,cusr2,len1,len-1);

    cusr = NGFstrToCstr(cusr2);
    cusr[len-1] = '\0';
    for( i = len-2; i >= 0; i-- ) {
        if( cusr[i] != ' ' && cusr[i] != '\0' ) {
            cusr[i+1] = '\0';
            break;
        }
    }
}
