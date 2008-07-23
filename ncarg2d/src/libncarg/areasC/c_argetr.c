/*
 *	$Id: c_argetr.c,v 1.5 2008-07-23 16:16:39 haley Exp $
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

extern void NGCALLF(argetr,ARGETR)(NGstring,float*,int);

void c_argetr
#ifdef NeedFuncProto
(
    char *ipn,
    float *rvl
)
#else
(ipn,rvl)
    char *ipn;
    float *rvl;
#endif
{
	NGstring ipn2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !ipn ) { 
        fprintf( stderr, "c_argetr:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(ipn);
	ipn2 = NGCstrToFstr(ipn,len);
    NGCALLF(argetr,ARGETR)(ipn2,rvl,len);
}
