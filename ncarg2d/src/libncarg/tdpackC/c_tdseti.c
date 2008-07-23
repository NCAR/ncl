/*
 *      $Id: c_tdseti.c,v 1.5 2008-07-23 16:17:06 haley Exp $
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

extern void NGCALLF(tdseti,TDSETI)(NGstring,int*,int);

void c_tdseti
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
        fprintf( stderr, "c_tdseti:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(pnam);
    pnam2 = NGCstrToFstr(pnam,len);
    NGCALLF(tdseti,TDSETI)(pnam2,&ival,len);
}
