/*
 *	$Id: c_nggeti.c,v 1.5 2008-07-23 16:16:57 haley Exp $
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

extern void NGCALLF(nggeti,NGGETI)(NGstring,int*,int);

void c_nggeti
#ifdef NeedFuncProto
(
    char *pnam,
    int *ival
)
#else
(pnam,ival)
    char *pnam;
    int *ival;
#endif
{
    NGstring pnam2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if ( !pnam) {
        fprintf( stderr, "c_nggeti:  illegal parameter name (NULL)\n");
        return;
    }
    len = NGSTRLEN(pnam);
    pnam2 = NGCstrToFstr(pnam,len);
    NGCALLF(nggeti,NGGETI)(pnam2,ival,len);
}
