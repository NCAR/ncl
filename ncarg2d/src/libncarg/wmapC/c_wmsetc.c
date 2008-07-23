/*
 *	$Id: c_wmsetc.c,v 1.5 2008-07-23 16:17:09 haley Exp $
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

extern void NGCALLF(wmsetc,WMSETC)(NGstring,NGstring,int,int);

void c_wmsetc
#ifdef NeedFuncProto
(
    char *cnp,
    char *cvp
)
#else
( cnp, cvp )
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
        fprintf( stderr, "c_wmsetc:  illegal parameter name (NULL)\n" );
        return;
    }
    len1 = NGSTRLEN(cnp);
    len2 = NGSTRLEN(cvp);
    cnp2 = NGCstrToFstr(cnp,len1);
    cvp2 = NGCstrToFstr(cvp,len2);
    NGCALLF(wmsetc,WMSETC)(cnp2,cvp2,len1,len2);
}


