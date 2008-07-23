/*
 *	$Id: c_pcdlsc.c,v 1.5 2008-07-23 15:46:49 haley Exp $
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

extern void NGCALLF(pcdlsc,PCDLSC)(int*);

void c_pcdlsc 
#ifdef NeedFuncProto
(
    int ifci
)
#else
 (ifci)
    int ifci;
#endif
{
    NGCALLF(pcdlsc,PCDLSC)(&ifci);
}

