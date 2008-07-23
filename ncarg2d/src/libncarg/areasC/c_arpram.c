/*
 *	$Id: c_arpram.c,v 1.5 2008-07-23 16:16:40 haley Exp $
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

extern void NGCALLF(arpram,ARPRAM)(int*,int*,int*,int*);

void c_arpram
#ifdef NeedFuncProto
(
    int *iam,
    int if1,
    int if2,
    int if3
)
#else
(iam,if1,if2,if3)
    int *iam;
    int if1;
    int if2;
    int if3;
#endif
{
    NGCALLF(arpram,ARPRAM)(iam,&if1,&if2,&if3);
}
