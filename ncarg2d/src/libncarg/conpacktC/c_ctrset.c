/*
 *      $Id: c_ctrset.c,v 1.3 2008-07-23 16:16:44 haley Exp $
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

extern void NGCALLF(ctrset,CTRSET)();

void c_ctrset()
{
    NGCALLF(ctrset,CTRSET)();
}
