/*
 *	$Id: c_dpseti.c,v 1.5 2008-07-23 16:16:46 haley Exp $
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

extern void NGCALLF(dpseti,DPSETI)(NGstring,int*,int);

void c_dpseti
#ifdef NeedFuncProto
(
    char *pnam,
    int ival
)
#else
(pnam,ival)
    char *pnam;
    int ival;
#endif
{
	NGstring pnam2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !pnam ) { 
        fprintf( stderr, "c_dpseti:  illegal parameter string (NULL)\n" );
        return;
    }

    len = NGSTRLEN(pnam);
	pnam2 = NGCstrToFstr(pnam,len);
    NGCALLF(dpseti,DPSETI)(pnam2,&ival,len);
}
