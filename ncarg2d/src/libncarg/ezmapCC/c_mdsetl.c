/*
 *      $Id: c_mdsetl.c,v 1.2 2008-07-23 16:16:52 haley Exp $
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

extern void NGCALLF(mdsetl,MDSETL)(NGstring,int*,int);

void c_mdsetl
#ifdef NeedFuncProto
(
    char *whch,
    int lval
)
#else
(whch,lval)
    char *whch;
    int lval;
#endif
{
    NGstring whch2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !whch ) { 
	fprintf( stderr, "c_mdsetl:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(whch);
    whch2 = NGCstrToFstr(whch,len);
    NGCALLF(mdsetl,MDSETL)(whch2,&lval,len);
}
