/*
 *	$Id: c_mpsetr.c,v 1.2 2008-07-23 16:16:54 haley Exp $
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

extern void NGCALLF(mpsetr,MPSETR)(NGstring,float*,int);

void c_mpsetr
#ifdef NeedFuncProto
(
    char *whch,
    float rval
)
#else
(whch,rval)
    char *whch;
    float rval;
#endif
{
    NGstring whch2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !whch ) { 
        fprintf( stderr, "c_mpsetr:  illegal parameter string (NULL)\n" );
        return;
    }

    len = NGSTRLEN(whch);
    whch2 = NGCstrToFstr(whch,len);
    NGCALLF(mpsetr,MPSETR)(whch2,&rval,len);
}
