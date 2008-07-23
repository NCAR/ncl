/*
 *	$Id: c_ezxy.c,v 1.5 2008-07-23 16:16:41 haley Exp $
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

extern void NGCALLF(ezxy,EZXY)(float*,float*,int*,NGstring,int);

void c_ezxy
#ifdef NeedFuncProto
(
    float *xdra,
    float *ydra,
    int npts,
    char *labg
)
#else
(xdra,ydra,npts,labg)
    float *xdra;
    float *ydra;
    int npts;
    char *labg;
#endif
{
	NGstring labg2;
    int len;

    len = NGSTRLEN(labg);
	labg2 = NGCstrToFstr(labg,len);
    NGCALLF(ezxy,EZXY)(xdra,ydra,&npts,labg2,len);
}
