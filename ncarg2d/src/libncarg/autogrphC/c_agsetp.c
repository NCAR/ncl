/*
 *	$Id: c_agsetp.c,v 1.5 2008-07-23 16:16:41 haley Exp $
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

extern void NGCALLF(agsetp,AGSETP)(NGstring,float*,int*,int);

void c_agsetp
#ifdef NeedFuncProto
(
    char *tpid,
    float *fura,
    int lura
)
#else
(tpid,fura,lura)
    char *tpid;
    float *fura;
    int lura;
#endif
{
    NGstring tpid2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !tpid ) { 
        fprintf( stderr, "c_agsetp:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(tpid);
    tpid2 = NGCstrToFstr(tpid,len);
    NGCALLF(agsetp,AGSETP)(tpid2,fura,&lura,len);
}
