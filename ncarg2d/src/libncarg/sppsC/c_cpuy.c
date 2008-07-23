/*
 *	$Id: c_cpuy.c,v 1.4 2008-07-23 16:17:01 haley Exp $
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

float c_cpuy
#ifdef NeedFuncProto
(
    int iy
)
#else
(iy)
    int iy;
#endif
{
    float y;
    y = NGCALLF(cpuy,CPUY)(&iy);
    return(y);
}
