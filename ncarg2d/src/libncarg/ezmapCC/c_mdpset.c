/*
 *      $Id: c_mdpset.c,v 1.2 2008-07-23 16:16:51 haley Exp $
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

extern void NGCALLF(mdpset,MDPSET)(NGstring,double*,double*,double*,double*,int);

void c_mdpset
#ifdef NeedFuncProto
(
    char *str,
    double *arg2,
    double *arg3,
    double *arg4,
    double *arg5
)
#else
(str,arg2,arg3,arg4,arg5)
    char *str;
    double *arg2;
    double *arg3;
    double *arg4;
    double *arg5;
#endif
{
    NGstring str2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !str ) { 
	fprintf( stderr, "c_mdpset:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(str);
    str2 = NGCstrToFstr(str,len);
    NGCALLF(mdpset,MDPSET)(str2,arg2,arg3,arg4,arg5,len);
}
