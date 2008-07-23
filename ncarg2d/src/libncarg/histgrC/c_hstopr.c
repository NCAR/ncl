/*
 *	$Id: c_hstopr.c,v 1.5 2008-07-23 16:16:56 haley Exp $
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

extern void NGCALLF(hstopr,HSTOPR)(NGstring,float*,int*,int);

void c_hstopr
#ifdef NeedFuncProto
(
    char *iopt,
    float *array,
    int isize
)
#else
(iopt,array,isize)
    char *iopt;
    float *array;
    int isize;
#endif
{
    NGstring iopt2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !iopt ) {
        fprintf( stderr, "c_hstopr:  illegal parameter name (NULL)\n" );
        return;
    }
    len = NGSTRLEN(iopt);
    iopt2 = NGCstrToFstr(iopt,len);
    NGCALLF(hstopr,HSTOPR)(iopt2,array,&isize,len);
}
