/*
 *	$Id: c_wmstnm.c,v 1.5 2008-07-23 16:17:09 haley Exp $
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

extern void NGCALLF(wmstnm,WMSTNM)(float*,float*,NGstring,int);

void c_wmstnm
#ifdef NeedFuncProto
(
    float x,
    float y,
    char *imdat
)
#else
(x,y,imdat)
    float x;
    float y;
    char *imdat;
#endif
{
    NGstring imdat2;
	int len1;
    len1 = NGSTRLEN(imdat);
    imdat2 = NGCstrToFstr(imdat,len1);
    NGCALLF(wmstnm,WMSTNM)(&x,&y,imdat2,len1);
}
