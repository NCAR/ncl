/*
 *	$Id: c_aggetr.c,v 1.5 2008-07-23 16:16:40 haley Exp $
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

extern void NGCALLF(aggetr,AGGETR)(NGstring,float*,int);

void c_aggetr
#ifdef NeedFuncProto
(
    char *tpid,
    float *fusr
)
#else
(tpid,fusr)
    char *tpid;
    float *fusr;
#endif
{
    NGstring tpid2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !tpid ) { 
        fprintf( stderr, "c_aggetr:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(tpid);
    tpid2 = NGCstrToFstr(tpid,len);
    NGCALLF(aggetr,AGGETR)(tpid2,fusr,len);
}
