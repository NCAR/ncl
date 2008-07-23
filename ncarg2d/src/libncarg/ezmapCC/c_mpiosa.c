/*
 *      $Id: c_mpiosa.c,v 1.2 2008-07-23 16:16:53 haley Exp $
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

extern int NGCALLF(mpiosa,MPIOSA)(int*,int*);

int c_mpiosa
#ifdef NeedFuncProto
(
    int iaid,
    int ilvl
)
#else
(iaid,ilvl)
    int iaid;
    int ilvl;
#endif
{
    return(NGCALLF(mpiosa,MPIOSA)(&iaid,&ilvl));
}
