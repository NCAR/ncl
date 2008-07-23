/*
 *      $Id: c_mdpvec.c,v 1.2 2008-07-23 16:16:51 haley Exp $
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

extern void NGCALLF(mdpvec,MDPVEC)(double*,double*);

void c_mdpvec
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
    NGCALLF(mdpvec,MDPVEC)(&xlat,&xlon);
}
