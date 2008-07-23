/*
 *	$Id: c_sfsetc.c,v 1.5 2008-07-23 16:17:00 haley Exp $
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

extern void NGCALLF(sfsetc,SFSETC)(NGstring,NGstring,int,int);

void c_sfsetc
#ifdef NeedFuncProto
(
    char *cnp,
    char *cvp
)
#else
(cnp,cvp)
    char *cnp;
    char *cvp;
#endif
{
    NGstring cnp2;
    NGstring cvp2;
    int len1, len2;
/*
 * Make sure parameter name is not NULL
 */
    if( !cnp ) {
        fprintf( stderr, "c_sfsetc:  illegal parameter name (NULL)\n");
        return;
    }
    len1 = NGSTRLEN(cnp);
    len2 = NGSTRLEN(cvp);
    cnp2 = NGCstrToFstr(cnp,len1);
    cvp2 = NGCstrToFstr(cvp,len2);
    NGCALLF(sfsetc,SFSETC)(cnp2,cvp2,len1,len2);
}
