/*
 *	$Id: c_arinam.c,v 1.5 2008-07-23 16:16:39 haley Exp $
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

extern void NGCALLF(arinam,ARINAM)(int*,int*);

void c_arinam
#ifdef NeedFuncProto
(
    int *iam,
    int lam
)
#else
(iam,lam)
    int *iam;
    int lam;
#endif
{
    NGCALLF(arinam,ARINAM)(iam,&lam);
}
