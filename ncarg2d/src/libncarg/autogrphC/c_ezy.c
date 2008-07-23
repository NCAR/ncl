/*
 *	$Id: c_ezy.c,v 1.5 2008-07-23 16:16:41 haley Exp $
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

extern void NGCALLF(ezy,EZY)(float*,int*,NGstring,int);

void c_ezy
#ifdef NeedFuncProto
(
    float *ydra,
    int npts,
    char *labg
)
#else
(ydra,npts,labg)
    float *ydra;
    int npts;
    char *labg;
#endif
{
    NGstring labg2;
    int len;

    len = NGSTRLEN(labg);
    labg2 = NGCstrToFstr(labg,len);
    NGCALLF(ezy,EZY)(ydra,&npts,labg2,len);
}
