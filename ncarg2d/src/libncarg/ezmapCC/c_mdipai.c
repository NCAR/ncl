/*
 *      $Id: c_mdipai.c,v 1.2 2008-07-23 16:16:49 haley Exp $
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

extern int NGCALLF(mdipai,MDIPAI)(int*,int*);

int c_mdipai
#ifdef NeedFuncProto
(
    int iain,
    int iaip
)
#else
(iain,iaip)
    int iain;
    int iaip;
#endif
{
    return(NGCALLF(mdipai,MDIPAI)(&iain,&iaip));
}
