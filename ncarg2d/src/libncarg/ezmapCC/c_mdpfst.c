/*
 *      $Id: c_mdpfst.c,v 1.2 2008-07-23 16:16:50 haley Exp $
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

extern void NGCALLF(mdpfst,MDPFST)(double*,double*);

void c_mdpfst
#ifdef NeedFuncProto
(
    double xlat,
    double xlon
)
#else
(xlat,xlon)
    double xlat;
    double xlon;
#endif
{
    NGCALLF(mdpfst,MDPFST)(&xlat,&xlon);
}
