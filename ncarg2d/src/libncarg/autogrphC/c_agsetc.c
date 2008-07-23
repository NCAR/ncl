/*
 *	$Id: c_agsetc.c,v 1.5 2008-07-23 16:16:40 haley Exp $
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

extern void NGCALLF(agsetc,AGSETC)(NGstring,NGstring,int,int);

void c_agsetc
#ifdef NeedFuncProto
(
    char *tpid,
    char *cusr
)
#else
(tpid,cusr)
    char *tpid;
    char *cusr;
#endif
{
    NGstring tpid2;
    NGstring cusr2;
    int len1, len2;
/*
 * Make sure parameter name is not NULL
 */
    if( !tpid ) { 
        fprintf( stderr, "c_agsetc:  illegal parameter string (NULL)\n" );
        return;
    }
/*
 * Make sure return string is not NULL
 */
    if( !cusr ) { 
        fprintf( stderr, "c_agsetc:  illegal return string (NULL)\n" );
        return;
    }
    len1 = NGSTRLEN(tpid);
    len2 = NGSTRLEN(cusr);
    tpid2 = NGCstrToFstr(tpid,len1);
    cusr2 = NGCstrToFstr(cusr,len2);
    NGCALLF(agsetc,AGSETC)(tpid2,cusr2,len1,len2);
}
