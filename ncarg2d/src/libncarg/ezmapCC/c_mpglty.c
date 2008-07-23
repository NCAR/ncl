/*
 *      $Id: c_mpglty.c,v 1.2 2008-07-23 16:16:53 haley Exp $
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

extern void NGCALLF(mpglty,MPGLTY)(int*);

void c_mpglty
#ifdef NeedFuncProto
(
    int *ilty
)
#else
(ilty)
    int *ilty;
#endif
{
    NGCALLF(mpglty,MPGLTY)(ilty);
}
