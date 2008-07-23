/*
 *	$Id: c_gacolr.c,v 1.5 2008-07-23 16:16:55 haley Exp $
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

extern void NGCALLF(gacolr,GACOLR)(int*,int*,int*,int*);

void c_gacolr
#ifdef NeedFuncProto
(
    int kaxs,
    int klbl,
    int kmjt,
    int kmnt
)
#else
(kaxs,klbl,kmjt,kmnt)
    int kaxs;
    int klbl;
    int kmjt;
    int kmnt;
#endif
{
    NGCALLF(gacolr,GACOLR)(&kaxs,&klbl,&kmjt,&kmnt);
}
