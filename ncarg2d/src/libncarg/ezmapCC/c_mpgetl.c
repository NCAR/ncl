/*
 *	$Id: c_mpgetl.c,v 1.2 2008-07-23 16:16:53 haley Exp $
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

extern void NGCALLF(mpgetl,MPGETL)(NGstring,int*,int);

void c_mpgetl
#ifdef NeedFuncProto
(
    char *whch,
    int *lval
)
#else
(whch,lval)
    char *whch;
    int *lval;
#endif
{
    NGstring whch2;
    int len;
    int lval2;
/*
 * Make sure parameter name is not NULL
 */
    if( !whch ) { 
        fprintf( stderr, "c_mpgetl:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(whch);
    whch2 = NGCstrToFstr(whch,len);
    lval2 = NGClgclToFlgcl(*lval);
    NGCALLF(mpgetl,MPGETL)(whch2,&lval2,len);
    *lval = NGFlgclToClgcl(lval2);
}
