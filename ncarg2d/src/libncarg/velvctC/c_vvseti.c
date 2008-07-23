/*
 *	$Id: c_vvseti.c,v 1.5 2008-07-23 16:17:08 haley Exp $
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

extern void NGCALLF(vvseti,VVSETI)(NGstring,int*,int);

void c_vvseti
#ifdef NeedFuncProto
(
    char *whch,
    int ival
)
#else
(whch,ival)
    char *whch;
    int ival;
#endif
{
    NGstring whch2;
    int len;
/* 
 * Make sure parameter name is not NULL
 */
    if( !whch ) {
         fprintf( stderr, "c_vvseti:  illegal parameter name (NULL)\n" );
         return;
    }
    len = NGSTRLEN(whch);
    whch2 = NGCstrToFstr(whch,len);
    NGCALLF(vvseti,VVSETI)(whch2,&ival,len);
}
