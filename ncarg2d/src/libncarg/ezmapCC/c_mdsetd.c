/*
 *      $Id: c_mdsetd.c,v 1.2 2008-07-23 16:16:52 haley Exp $
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

extern void NGCALLF(mdsetd,MDSETD)(NGstring,double*,int);

void c_mdsetd
#ifdef NeedFuncProto
(
    char *whch,
    double dval
)
#else
(whch,dval)
    char *whch;
    double dval;
#endif
{
    NGstring whch2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !whch ) { 
	fprintf( stderr, "c_mdsetd:  illegal parameter string (NULL)\n" );
        return;
    }

    len = NGSTRLEN(whch);
    whch2 = NGCstrToFstr(whch,len);
    NGCALLF(mdsetd,MDSETD)(whch2,&dval,len);
}
