/*
 *      $Id: c_mpiaty.c,v 1.2 2008-07-23 16:16:53 haley Exp $
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

extern int NGCALLF(mpiaty,MPIATY)(int*);

int c_mpiaty
#ifdef NeedFuncProto
(
    int iain
)
#else
(iain)
    int iain;
#endif
{
    return(NGCALLF(mpiaty,MPIATY)(&iain));
}
