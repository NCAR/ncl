/*
 *	$Id: c_arseti.c,v 1.5 2008-07-23 16:16:40 haley Exp $
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

extern void NGCALLF(arseti,ARSETI)(NGstring,int*,int);

void c_arseti
#ifdef NeedFuncProto
(
    char *ipn,
    int ivl
)
#else
(ipn,ivl)
    char *ipn;
    int ivl;
#endif
{
	NGstring ipn2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !ipn ) { 
        fprintf( stderr, "c_arseti:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(ipn);
	ipn2 = NGCstrToFstr(ipn,len);
    NGCALLF(arseti,ARSETI)(ipn2,&ivl,len);
}
