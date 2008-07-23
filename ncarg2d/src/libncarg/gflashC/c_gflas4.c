/*
 *	$Id: c_gflas4.c,v 1.5 2008-07-23 16:16:55 haley Exp $
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

extern void NGCALLF(gflas4,GFLAS4)(int*,NGstring,int);

void c_gflas4
#ifdef NeedFuncProto
(
    int id,
    char *fname
)
#else
(id,fname)
    int id;
    char *fname;
#endif
{
    NGstring fname2;
    int len;
/*
 * Make sure filename is not NULL
 */
    if( !fname ) {
        fprintf( stderr, "c_gflas4:  illegal filename (NULL)\n" );
        return;
    }
    len = NGSTRLEN(fname);
    fname2 = NGCstrToFstr(fname,len);
    NGCALLF(gflas4,GFLAS4)(&id,fname2,len);
}
