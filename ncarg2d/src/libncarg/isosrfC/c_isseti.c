/*
 *	$Id: c_isseti.c,v 1.5 2008-07-23 16:16:56 haley Exp $
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

extern void NGCALLF(isseti,ISSETI)(NGstring,int*,int);

void c_isseti
#ifdef NeedFuncProto
(
    char *ipn,
    int ivl
)
#else
(ipn,ivl)
    char *ipn;
    int ivl;
#endif
{
    NGstring ipn2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !ipn ) {
        fprintf( stderr, "c_isseti: illegal parameter name (NULL)\n" );
        return;
    }
    len = NGSTRLEN(ipn);
    ipn2 = NGCstrToFstr(ipn,len);
    NGCALLF(isseti,ISSETI)(ipn2,&ivl,len);
}
