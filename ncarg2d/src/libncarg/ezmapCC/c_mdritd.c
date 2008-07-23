/*
 *      $Id: c_mdritd.c,v 1.2 2008-07-23 16:16:51 haley Exp $
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

extern void NGCALLF(mdritd,MDRITD)(int*,double*,double*,double*,double*);

void c_mdritd
#ifdef NeedFuncProto
(
    int iaxs,
    double angl,
    double *ucrd,
    double *vcrd,
    double *wcrd
)
#else
(iaxs,angl,ucrd,vcrd,wcrd)
    int iaxs;
    double angl;
    double *ucrd;
    double *vcrd;
    double *wcrd;
#endif
{
    NGCALLF(mdritd,MDRITD)(&iaxs,&angl,ucrd,vcrd,wcrd);
}
