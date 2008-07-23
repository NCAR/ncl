/*
 *	$Id: c_kmpx.c,v 1.4 2008-07-23 16:17:01 haley Exp $
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

int c_kmpx
#ifdef NeedFuncProto
(
    int ix
)
#else
(ix)
    int ix;
#endif
{
	return(NGCALLF(kmpx,KMPX)(&ix));
}
