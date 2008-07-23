/*
 *	$Id: c_sfgeti.c,v 1.5 2008-07-23 16:16:59 haley Exp $
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

extern void NGCALLF(sfgeti,SFGETI)(NGstring,int*,int);

void c_sfgeti
#ifdef NeedFuncProto
(
    char *cnp,
    int *ivp
)
#else
(cnp,ivp)
    char *cnp;
    int *ivp;
#endif
{
    NGstring cnp2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !cnp ) {
        fprintf( stderr, "c_sfgeti:  illegal parameter name (NULL)\n");
        return;
    }
    len = NGSTRLEN(cnp);
    cnp2 = NGCstrToFstr(cnp,len);
    NGCALLF(sfgeti,SFGETI)(cnp2,ivp,len);
}
