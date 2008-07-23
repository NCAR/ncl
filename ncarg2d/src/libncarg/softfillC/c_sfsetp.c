/*
 *	$Id: c_sfsetp.c,v 1.5 2008-07-23 16:17:00 haley Exp $
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

extern void NGCALLF(sfsetp,SFSETP)(int[8][8]);

void c_sfsetp
#ifdef NeedFuncProto
(
    int idp[8][8]
)
#else
(idp)
    int idp[8][8];
#endif
{
    NGCALLF(sfsetp,SFSETP)(idp);
}
