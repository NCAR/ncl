/*
 *	$Id: c_gasetr.c,v 1.5 2008-07-23 16:16:55 haley Exp $
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

extern void NGCALLF(gasetr,GASETR)(NGstring,float*,int);

void c_gasetr
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
 *  Make sure parameter name is not NULL
 */
    if( !pnam ) {
        fprintf( stderr, "c_gasetr:  illegal parameter string (NULL)\n");
        return;
    }

    len = NGSTRLEN(pnam);
    pnam2 = NGCstrToFstr(pnam,len);
    NGCALLF(gasetr,GASETR)(pnam2,&rval,len);
}
