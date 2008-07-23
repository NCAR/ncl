/*
 *	$Id: c_agsetr.c,v 1.5 2008-07-23 16:16:41 haley Exp $
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

extern void NGCALLF(agsetr,AGSETR)(NGstring,float*,int);

void c_agsetr
#ifdef NeedFuncProto
(
    char *tpid,
    float fusr
)
#else
(tpid,fusr)
    char *tpid;
    float fusr;
#endif
{
    NGstring tpid2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !tpid ) { 
        fprintf( stderr, "c_agsetr:  illegal parameter string (NULL)\n" );
        return;
    }

    len = NGSTRLEN(tpid);
    tpid2 = NGCstrToFstr(tpid,len);
    NGCALLF(agsetr,AGSETR)(tpid2,&fusr,len);
}
