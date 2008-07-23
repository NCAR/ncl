/*
 *	$Id: c_ngsetr.c,v 1.5 2008-07-23 16:16:58 haley Exp $
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

extern void NGCALLF(ngsetr,NGSETR)(NGstring,float*,int);

void c_ngsetr
#ifdef NeedFuncProto
(
    char *pnam,
    float rval
)
#else
(pnam,rval)
    char *pnam;
    float rval;
#endif
{
    NGstring pnam2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !pnam ) { 
        fprintf( stderr, "c_ngsetr:  illegal parameter name (NULL)\n" );
        return;
    }

    len = NGSTRLEN(pnam);
    pnam2 = NGCstrToFstr(pnam,len);
    NGCALLF(ngsetr,NGSETR)(pnam2,&rval,len);
}
