/*
 *	$Id: c_gasetc.c,v 1.5 2008-07-23 16:16:55 haley Exp $
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

extern void NGCALLF(gasetc,GASETC)(NGstring,NGstring,int,int);


void c_gasetc
#ifdef NeedFuncProto
(
    char *pnam,
    char *cval
)
#else
(pnam,cval)
    char *pnam;
    char *cval;
#endif
{
    NGstring pnam2;
    NGstring cval2;
    int len1, len2;
/*
 *  Make sure parameter name is not NULL
 */
    if( !pnam ) {
        fprintf( stderr, "c_gasetc:  illegal parameter string (NULL)\n");
        return;
    }
/*
 *  Make sure return string is not NULL
 */
    if( !cval ) {
        fprintf( stderr, "c_gasetc:  illegal parameter string (NULL)\n");
        return;
    }
    len1 = NGSTRLEN(pnam);
    len2 = NGSTRLEN(cval);
    pnam2 = NGCstrToFstr(pnam,len1);
    cval2 = NGCstrToFstr(cval,len2);
    NGCALLF(gasetc,GASETC)(pnam2,cval2,len1,len2);
}
