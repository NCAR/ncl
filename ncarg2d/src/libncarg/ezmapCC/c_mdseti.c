/*
 *      $Id: c_mdseti.c,v 1.2 2008-07-23 16:16:52 haley Exp $
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

#define MAX 100

extern void NGCALLF(mdseti,MDSETI)(NGstring,int*,int);

void c_mdseti
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
    int array[MAX];
/*
 * Make sure parameter name is not NULL
 */
    if( !whch ) { 
	fprintf( stderr, "c_mdseti:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(whch);
    whch2 = NGCstrToFstr(whch,len);
    NGCALLF(mdseti,MDSETI)(whch2,&ival,len);
}
