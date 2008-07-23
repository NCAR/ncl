/*
 *	$Id: c_armvam.c,v 1.5 2008-07-23 16:16:40 haley Exp $
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

extern void NGCALLF(armvam,ARMVAM)(int*,int*,int*);

void c_armvam
#ifdef NeedFuncProto
(
    int *iam,
    int *ian,
    int lan
)
#else
(iam,ian,lan)
    int *iam;
    int *ian;
    int lan;
#endif
{
    NGCALLF(armvam,ARMVAM)(iam,ian,&lan);
}
