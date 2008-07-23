/*
 *	$Id: c_cpux.c,v 1.4 2008-07-23 16:17:01 haley Exp $
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

float c_cpux
#ifdef NeedFuncProto
(
    int ix
)
#else
(ix)
    int ix;
#endif
{
    float x;
    x = NGCALLF(cpux,CPUX)(&ix);
    return(x);
}
