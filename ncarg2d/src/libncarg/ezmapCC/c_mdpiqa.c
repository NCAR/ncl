/*
 *      $Id: c_mdpiqa.c,v 1.2 2008-07-23 16:16:50 haley Exp $
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

extern void NGCALLF(mdpiqa,MDPIQA)(int*,int*,int*,int*);

void c_mdpiqa
#ifdef NeedFuncProto
(
    int *iamp,
    int igrp,
    int idlt,
    int idrt
)
#else
( iamp, igrp, idlt, idrt )
    int *iamp;
    int igrp;
    int idlt;
    int idrt;
#endif
{
      NGCALLF(mdpiqa,MDPIQA)( iamp, &igrp, &idlt, &idrt );
}
