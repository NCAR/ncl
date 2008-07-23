/*
 *	$Id: c_wmlabs.c,v 1.5 2008-07-23 16:17:09 haley Exp $
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

extern void NGCALLF(wmlabs,WMLABS)(float*,float*,NGstring,int);

void c_wmlabs
#ifdef NeedFuncProto
(
    float x,
    float y,
    char *symtyp
)
#else
(x,y,symtyp)
    float x;
    float y;
    char *symtyp;
)
#endif
{
    NGstring symtyp2;
	int len;
	len = NGSTRLEN(symtyp);
	symtyp2 = NGCstrToFstr(symtyp,len);
    NGCALLF(wmlabs,WMLABS)(&x,&y,symtyp2,len);
}
