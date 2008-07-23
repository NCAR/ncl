/*
 *	$Id: c_slseti.c,v 1.5 2008-07-23 16:17:03 haley Exp $
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

extern void NGCALLF(slseti,SLSETI)(NGstring,int*,int);

void c_slseti
#ifdef NeedFuncProto
(
    char *pa,
    int ival
)
#else
(pa,ival)
    char *pa;
    int ival;
#endif
{
    NGstring pa2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !pa ) {
        fprintf( stderr, "c_slseti:  illegal parameter name (NULL)\n" );
        return;
    }
    len = NGSTRLEN(pa);
    pa2 = NGCstrToFstr(pa,len);
    NGCALLF(slseti,SLSETI)(pa2,&ival,len);
}
