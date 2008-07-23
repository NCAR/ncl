/*
 *	$Id: c_ngsetc.c,v 1.5 2008-07-23 16:16:58 haley Exp $
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

extern void NGCALLF(ngsetc,NGSETC)(NGstring,NGstring,int,int);

void c_ngsetc
#ifdef NeedFuncProto
(
    char *pnam,
    char *cvp
)
#else
( pnam, cvp )
    char *pnam;
    char *cvp;
#endif
{
    NGstring pnam2;
    NGstring cvp2;
    int len1, len2;
/*
 * Make sure parameter name is not NULL
 */
    if( !pnam ) { 
        fprintf( stderr, "c_ngsetc:  illegal parameter name (NULL)\n" );
        return;
    }
    len1 = NGSTRLEN(pnam);
    len2 = NGSTRLEN(cvp);
    pnam2 = NGCstrToFstr(pnam,len1);
    cvp2 = NGCstrToFstr(cvp,len2);
    NGCALLF(ngsetc,NGSETC)(pnam2,cvp2,len1,len2);
}


