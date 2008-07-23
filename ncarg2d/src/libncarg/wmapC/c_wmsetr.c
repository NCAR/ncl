/*
 *	$Id: c_wmsetr.c,v 1.5 2008-07-23 16:17:09 haley Exp $
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

extern void NGCALLF(wmsetr,WMSETR)(NGstring,float*,int);

void c_wmsetr
#ifdef NeedFuncProto
(
    char *cnp,
    float rvp
)
#else
(cnp,rvp)
    char *cnp;
    float rvp;
#endif
{
    NGstring cnp2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !cnp ) { 
        fprintf( stderr, "c_wmsetr:  illegal parameter name (NULL)\n" );
        return;
    }

    len = NGSTRLEN(cnp);
    cnp2 = NGCstrToFstr(cnp,len);
    NGCALLF(wmsetr,WMSETR)(cnp2,&rvp,len);
}
