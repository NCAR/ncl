/*
 *	$Id: c_slsetr.c,v 1.5 2008-07-23 16:17:03 haley Exp $
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

extern void NGCALLF(slsetr,SLSETR)(NGstring,float*,int);

void c_slsetr
#ifdef NeedFuncProto
(
    char *pa,
    float rval
)
#else
(pa,rval)
    char *pa;
    float rval;
#endif
{
    NGstring pa2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !pa ) {
        fprintf( stderr, "c_slsetr:  illegal parameter name (NULL)\n" );
        return;
    }

    len = NGSTRLEN(pa);
    pa2 = NGCstrToFstr(pa,len);
    NGCALLF(slsetr,SLSETR)(pa2,&rval,len);
}
