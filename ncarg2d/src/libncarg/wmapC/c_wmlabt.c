/*
 *	$Id: c_wmlabt.c,v 1.5 2008-07-23 16:17:09 haley Exp $
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

extern void NGCALLF(wmlabt,WMLABT)(float*,float*,NGstring,int*,int);

void c_wmlabt
#ifdef NeedFuncProto
(
    float x,
    float y,
    char *label,
    int iflg
)
#else
(x,y,label,iflg)
    float x;
    float y;
    char *label;
    int iflg;
)
#endif
{
    NGstring label2;
	int len;
	len = NGSTRLEN(label);
	label2 = NGCstrToFstr(label,len);
    NGCALLF(wmlabt,WMLABT)(&x,&y,label2,&iflg,len);
}
