/*
 *	$Id: c_aggeti.c,v 1.5 2008-07-23 16:16:40 haley Exp $
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

extern void NGCALLF(aggeti,AGGETI)(NGstring,int*,int);

void c_aggeti
#ifdef NeedFuncProto
(
    char *tpid,
    int *iusr
)
#else
(tpid,iusr)
    char *tpid;
    int *iusr;
#endif
{
    NGstring tpid2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !tpid ) { 
        fprintf( stderr, "c_aggeti:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(tpid);
    tpid2 = NGCstrToFstr(tpid,len);
    NGCALLF(aggeti,AGGETI)(tpid2,iusr,len);
}
