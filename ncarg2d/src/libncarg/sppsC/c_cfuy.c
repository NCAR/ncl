/*
 *	$Id: c_cfuy.c,v 1.5 2008-07-23 16:17:00 haley Exp $
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

float c_cfuy
#ifdef NeedFuncProto
(
    float ry
)
#else
(ry)
    float ry;
#endif
{
    float y;
    y = NGCALLF(cfuy,CFUY)(&ry);
    return(y);
}
