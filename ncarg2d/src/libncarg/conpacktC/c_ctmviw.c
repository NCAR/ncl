/*
 *      $Id: c_ctmviw.c,v 1.2 2008-07-23 16:16:44 haley Exp $
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

extern void NGCALLF(ctmviw,CTMVIW)(int*,int*,int*);

void c_ctmviw
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
    NGCALLF(ctmviw,CTMVIW)(iwko,iwrk,&lwkn);
}
