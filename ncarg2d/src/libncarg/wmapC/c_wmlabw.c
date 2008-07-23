/*
 *	$Id: c_wmlabw.c,v 1.5 2008-07-23 16:17:09 haley Exp $
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

extern void NGCALLF(wmlabw,WMLABW)(float*,float*,NGstring,int);

void c_wmlabw
#ifdef NeedFuncProto
(
    float x,
    float y,
    char *label
)
#else
(x,y,label)
    float x;
    float y;
    char *label;
)
#endif
{
    NGstring label2;
	int len;
	len = NGSTRLEN(label);
	label2 = NGCstrToFstr(label,len);
    NGCALLF(wmlabw,WMLABW)(&x,&y,label2,len);
}
