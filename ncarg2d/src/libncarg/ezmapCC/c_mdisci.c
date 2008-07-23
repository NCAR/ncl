/*
 *      $Id: c_mdisci.c,v 1.2 2008-07-23 16:16:49 haley Exp $
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

extern int NGCALLF(mdisci,MDISCI)(int*);

int c_mdisci
#ifdef NeedFuncProto
(
    int iain
)
#else
(iain)
    int iain;
#endif
{
    return(NGCALLF(mdisci,MDISCI)(&iain));
}
