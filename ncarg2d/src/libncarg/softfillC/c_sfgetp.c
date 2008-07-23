/*
 *	$Id: c_sfgetp.c,v 1.5 2008-07-23 16:16:59 haley Exp $
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

extern void NGCALLF(sfgetp,SFGETP)(int[8][8]);

void c_sfgetp
#ifdef NeedFuncProto
(
    int idp[8][8]
)
#else
(idp)
    int idp[8][8];
#endif
{
    NGCALLF(sfgetp,SFGETP)(idp);
}
