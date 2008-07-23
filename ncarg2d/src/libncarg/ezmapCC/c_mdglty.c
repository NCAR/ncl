/*
 *      $Id: c_mdglty.c,v 1.2 2008-07-23 16:16:49 haley Exp $
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

extern void NGCALLF(mdglty,MDGLTY)(int*);

void c_mdglty
#ifdef NeedFuncProto
(
    int *ilty
)
#else
(ilty)
    int *ilty;
#endif
{
    NGCALLF(mdglty,MDGLTY)(ilty);
}
