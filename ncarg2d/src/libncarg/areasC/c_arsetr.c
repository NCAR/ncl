/*
 *	$Id: c_arsetr.c,v 1.5 2008-07-23 16:16:40 haley Exp $
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

extern void NGCALLF(arsetr,ARSETR)(NGstring,float*,int);

void c_arsetr
#ifdef NeedFuncProto
(
    char *ipn,
    float rvl
)
#else
(ipn,rvl)
    char *ipn;
    float rvl;
#endif
{
	NGstring ipn2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !ipn ) { 
        fprintf( stderr, "c_arsetr:  illegal parameter string (NULL)\n" );
        return;
    }

    len = NGSTRLEN(ipn);
	ipn2 = NGCstrToFstr(ipn,len);
    NGCALLF(arsetr,ARSETR)(ipn2,&rvl,len);
}
