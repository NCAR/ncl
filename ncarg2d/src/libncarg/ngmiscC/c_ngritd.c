/*
 *	$Id: c_ngritd.c,v 1.5 2008-07-23 16:16:58 haley Exp $
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

extern void NGCALLF(ngritd,NGRITD)(int*,float*,float*,float*,float*);

void c_ngritd
#ifdef NeedFuncProto
(
    int iaxs,
    float angl,
    float *ucrd,
    float *vcrd,
    float *wcrd
)
#else
(iaxs,angl,ucrd,vcrd,wcrd)
    int iaxs;
    float angl;
    float *ucrd;
    float *vcrd;
    float *wcrd;
#endif
{
    NGCALLF(ngritd,NGRITD)(&iaxs,&angl,ucrd,vcrd,wcrd);
}
