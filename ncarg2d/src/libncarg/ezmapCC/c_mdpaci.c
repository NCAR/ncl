/*
 *      $Id: c_mdpaci.c,v 1.2 2008-07-23 16:16:50 haley Exp $
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

extern int NGCALLF(mdpaci,MDPACI)(int*);

int c_mdpaci
#ifdef NeedFuncProto
(
    int iai
)
#else
(iai)
    int iai;
#endif
{
    return(NGCALLF(mdpaci,MDPACI)(&iai));
}
