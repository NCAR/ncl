/*
 *	$Id: c_setusv.c,v 1.5 2008-07-23 16:17:02 haley Exp $
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

extern void NGCALLF(setusv,SETUSV)(NGstring,int*,int);

void c_setusv
#ifdef NeedFuncProto
(
    char *vn,
    int iv
)
#else
(vn,iv)
    char *vn;
    int iv;
#endif
{
    NGstring vn2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !vn ) {
        fprintf( stderr, "c_setusv:  illegal parameter name (NULL)\n" );
        return;
    }
    len = NGSTRLEN(vn);
    vn2 = NGCstrToFstr(vn,len);
    NGCALLF(setusv,SETUSV)(vn2,&iv,len);
}
