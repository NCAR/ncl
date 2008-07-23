/*
 *	$Id: c_hstopl.c,v 1.5 2008-07-23 16:16:56 haley Exp $
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

extern void NGCALLF(hstopl,HSTOPL)(NGstring,int);

void c_hstopl
#ifdef NeedFuncProto
(
    char *iopt
)
#else
(iopt)
    char *iopt;
#endif
{
    NGstring iopt2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !iopt ) {
        fprintf( stderr, "c_hstopl:  illegal parameter name (NULL)\n" );
        return;
    }
    len = NGSTRLEN(iopt);
    iopt2 = NGCstrToFstr(iopt,len);
    NGCALLF(hstopl,HSTOPL)(iopt2,len);
}
