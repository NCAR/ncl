/*
 *	$Id: c_hstopi.c,v 1.5 2008-07-23 16:16:56 haley Exp $
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

extern void NGCALLF(hstopi,HSTOPI)(NGstring,int*,int*,int*,int*,int);

void c_hstopi
#ifdef NeedFuncProto
(
    char *string,
    int param1,
    int param2,
    int *icol,
    int lcol
)
#else
(string, param1,param2,icol,lcol)
    char *string;
    int param1;
    int param2;
    int *icol;
    int lcol;
#endif
{
    NGstring string2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !string ) {
        fprintf( stderr, "c_hstopi:  illegal parameter name (NULL)\n" );
        return;
    }
    len = NGSTRLEN(string);
    string2 = NGCstrToFstr(string,len);
    NGCALLF(hstopi,HSTOPI)(string2,&param1,&param2,icol,&lcol,len);
}
