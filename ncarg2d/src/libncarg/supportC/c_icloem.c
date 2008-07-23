/*
 *	$Id: c_icloem.c,v 1.5 2008-07-23 16:17:04 haley Exp $
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

int c_icloem
#ifdef NeedFuncProto
(
    char *messg
)
#else
(messg)
    char *messg;
#endif
{
    NGstring messg2;
    int len;
    extern int NGCALLF(icloem,ICLOEM)(NGstring,int);
/*
 * Make sure message is not NULL
 */
    if( !messg ) { 
        fprintf( stderr, "c_icloem:  illegal message (NULL)\n" );
        return(0);
    }
    len = NGSTRLEN(messg);
    messg2 = NGCstrToFstr(messg,len);
    return(NGCALLF(icloem,ICLOEM)(messg2,len));
}

