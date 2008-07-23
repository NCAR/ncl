/*
 *	$Id: c_ezmy.c,v 1.5 2008-07-23 16:16:41 haley Exp $
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

extern void NGCALLF(ezmy,EZMY)(float*,int*,int*,int*,NGstring,int);

void c_ezmy
#ifdef NeedFuncProto
(
    float *ydra,
    int idxy,
    int many,
    int npts,
    char *labg
)
#else
(ydra,idxy,many,npts,labg)
    float *ydra;
    int idxy;
    int many;
    int npts;
    char *labg;
#endif
{
    NGstring labg2;
    int len;

    len = NGSTRLEN(labg);
    labg2 = NGCstrToFstr(labg,len);
    NGCALLF(ezmy,EZMY)(ydra,&idxy,&many,&npts,labg2,len);
}
