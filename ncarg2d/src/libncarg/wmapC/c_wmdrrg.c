/*
 *	$Id: c_wmdrrg.c,v 1.5 2008-07-23 16:17:08 haley Exp $
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

extern void NGCALLF(wmdrrg,WMDRRG)(int*,float*,float*,NGstring,int*,float*,
                                   float*,int);

void c_wmdrrg
#ifdef NeedFuncProto
(
    int n,
    float *x,
    float *y,
    char *itype,
    int nc,
    float *xc,
    float *yc
)
#else
(n,x,y,itype,nc,xc,yc)
    int n;
    float *x;
    float *y;
    char *itype;
    int nc;
    float *xc;
    float *yc
#endif
{
	NGstring itype2;
	int len;
	len = NGSTRLEN(itype);
	itype2 = NGCstrToFstr(itype,len);
    NGCALLF(wmdrrg,WMDRRG)(&n,x,y,itype2,&nc,xc,yc,len);
}
