/*
 *      $Id: c_mdpsav.c,v 1.2 2008-07-23 16:16:51 haley Exp $
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

extern void NGCALLF(mdpsav,MDPSAV)(int*);

void c_mdpsav
#ifdef NeedFuncProto
(
    int ifno
)
#else
(ifno)
    int ifno;
#endif
{
    NGCALLF(mdpsav,MDPSAV)(&ifno);
}
