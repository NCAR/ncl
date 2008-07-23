/*
 *	$Id: c_dpsetr.c,v 1.5 2008-07-23 16:16:46 haley Exp $
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

extern void NGCALLF(dpsetr,DPSETR)(NGstring,float*,int);

void c_dpsetr
#ifdef NeedFuncProto
(
    char *pnam,
    float rval
)
#else
(pnam,rval)
    char *pnam;
    float rval;
#endif
{
	NGstring pnam2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !pnam ) { 
        fprintf( stderr, "c_dpsetr:  illegal parameter string (NULL)\n" );
        return;
    }

    len = NGSTRLEN(pnam);
	pnam2 = NGCstrToFstr(pnam,len);
    NGCALLF(dpsetr,DPSETR)(pnam2,&rval,len);
}
