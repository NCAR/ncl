/*
 *      $Id: c_mdproj.c,v 1.2 2008-07-23 16:16:51 haley Exp $
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

extern void NGCALLF(mdproj,MDPROJ)(NGstring,double*,double*,double*,int);

void c_mdproj
#ifdef NeedFuncProto
(
    char *str,
    double arg2,
    double arg3,
    double arg4
)
#else
(str,arg2,arg3,arg4)
    char *str;
    double arg2;
    double arg3;
    double arg4;
#endif
{
    NGstring str2;
    int len;
/*
 * Make sure projection name is not NULL
 */
    if( !str ) {
	fprintf( stderr, "c_mdproj:  illegal projection name (NULL)\n" );
        return;
    }
    len = NGSTRLEN(str);
    str2 = NGCstrToFstr(str,len);
    NGCALLF(mdproj,MDPROJ)(str2,&arg2,&arg3,&arg4,len);
}
