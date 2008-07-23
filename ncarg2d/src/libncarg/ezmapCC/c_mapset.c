/*
 *	$Id: c_mapset.c,v 1.2 2008-07-23 16:16:48 haley Exp $
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

extern void NGCALLF(mapset,MAPSET)(NGstring,float*,float*,float*,float*,int);

void c_mapset
#ifdef NeedFuncProto
(
    char *str,
    float *arg2,
    float *arg3,
    float *arg4,
    float *arg5
)
#else
(str,arg2,arg3,arg4,arg5)
    char *str;
    float *arg2;
    float *arg3;
    float *arg4;
    float *arg5;
#endif
{
    NGstring str2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !str ) { 
        fprintf( stderr, "c_mapset:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(str);
    str2 = NGCstrToFstr(str,len);
    NGCALLF(mapset,MAPSET)(str2,arg2,arg3,arg4,arg5,len);
}
