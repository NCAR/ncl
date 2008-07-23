/*
 *	$Id: c_wmseti.c,v 1.5 2008-07-23 16:17:09 haley Exp $
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

extern void NGCALLF(wmseti,WMSETI)(NGstring,int*,int);

void c_wmseti
#ifdef NeedFuncProto
(
    char *cnp,
    int ivp
)
#else
(cnp,ivp)
    char *cnp;
    int ivp;
#endif
{
    NGstring cnp2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if ( !cnp ) {
        fprintf( stderr, "c_wmseti:  illegal parameter name (NULL)\n");
        return;
    }
    len = NGSTRLEN(cnp);
    cnp2 = NGCstrToFstr(cnp,len);
    NGCALLF(wmseti,WMSETI)(cnp2,&ivp,len);
}
