/*
 *	$Id: c_cpmviw.c,v 1.5 2008-07-23 16:16:43 haley Exp $
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

extern void NGCALLF(cpmviw,CPMVIW)(int*,int*,int*);

void c_cpmviw
#ifdef NeedFuncProto
(
    int *iwko,
    int *iwrk,
    int lwkn
)
#else
(iwko,iwrk,lwkn)
    int *iwko;
    int *iwrk;
    int lwkn;
#endif
{
    NGCALLF(cpmviw,CPMVIW)(iwko,iwrk,&lwkn);
}
