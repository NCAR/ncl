/*
 *      $Id: c_mdppos.c,v 1.2 2008-07-23 16:16:51 haley Exp $
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

extern void NGCALLF(mdppos,MDPPOS)(double*,double*,double*,double*);

void c_mdppos
#ifdef NeedFuncProto
(
    double arg1,
    double arg2,
    double arg3,
    double arg4
)
#else
(arg1,arg2,arg3,arg4)
    double arg1;
    double arg2;
    double arg3;
    double arg4;
#endif
{
    NGCALLF(mdppos,MDPPOS)(&arg1,&arg2,&arg3,&arg4);
}
