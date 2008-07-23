/*
 *	$Id: c_slgetr.c,v 1.5 2008-07-23 16:17:03 haley Exp $
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

extern void NGCALLF(slgetr,SLGETR)(NGstring,float*,int);

void c_slgetr
#ifdef NeedFuncProto
(
    char *pa,
    float *rval
)
#else
(pa,rval)
    char *pa;
    float *rval;
#endif
{
    NGstring pa2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !pa ) {
        fprintf( stderr, "c_slgetr:  illegal parameter name (NULL)\n" );
        return;
    }
    len = NGSTRLEN(pa);
    pa2 = NGCstrToFstr(pa,len);
    NGCALLF(slgetr,SLGETR)(pa2,rval,len);
}
