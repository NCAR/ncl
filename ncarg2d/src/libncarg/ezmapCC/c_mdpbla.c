/*
 *      $Id: c_mdpbla.c,v 1.2 2008-07-23 16:16:50 haley Exp $
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

extern void NGCALLF(mdpbla,MDPBLA)(int*);

void c_mdpbla
#ifdef NeedFuncProto
(
    int *iamp
)
#else
(iamp)
    int *iamp;
#endif
{
    NGCALLF(mdpbla,MDPBLA)(iamp);
}
