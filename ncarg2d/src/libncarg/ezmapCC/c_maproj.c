/*
 *	$Id: c_maproj.c,v 1.2 2008-07-23 16:16:48 haley Exp $
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

extern void NGCALLF(maproj,MAPROJ)(NGstring,float*,float*,float*,int);

void c_maproj
#ifdef NeedFuncProto
(
    char *str,
    float arg2,
    float arg3,
    float arg4
)
#else
(str,arg2,arg3,arg4)
    char *str;
    float arg2;
    float arg3;
    float arg4;
#endif
{
    NGstring str2;
    int len;
/*
 * Make sure projection name is not NULL
 */
    if( !str ) {
        fprintf( stderr, "c_maproj:  illegal projection name (NULL)\n" );
        return;
    }
    len = NGSTRLEN(str);
    str2 = NGCstrToFstr(str,len);
    NGCALLF(maproj,MAPROJ)(str2,&arg2,&arg3,&arg4,len);
}
