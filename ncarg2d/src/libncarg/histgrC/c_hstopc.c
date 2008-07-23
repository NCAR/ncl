/*
 *	$Id: c_hstopc.c,v 1.5 2008-07-23 16:16:56 haley Exp $
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

extern void NGCALLF(hstopc,HSTOPC)(NGstring,NGstring,int*,int*,int,int);

void c_hstopc
#ifdef NeedFuncProto
(
    char *iopt,
    char *string,
    int number,
    int ilch
)
#else
(iopt,string,number,ilch)
    char *iopt;
    char *string;
    int number;
    int ilch;
#endif
{
    NGstring iopt2;
    NGstring string2;
    int len1, len2;
/*
 * Make sure parameter name is not NULL
 */
    if( !iopt ) {
        fprintf( stderr, "c_hstopc:  illegal parameter name (NULL)\n" );
        return;
    }
    len1 = NGSTRLEN(iopt);
    len2 = NGSTRLEN(string);
    iopt2 = NGCstrToFstr(iopt,len1);
    string2 = NGCstrToFstr(string,len2);
    NGCALLF(hstopc,HSTOPC)(iopt2,string2,&number,&ilch,len1,len2);
}
