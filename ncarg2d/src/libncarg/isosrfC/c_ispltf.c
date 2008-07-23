/*
 *	$Id: c_ispltf.c,v 1.5 2008-07-23 16:16:56 haley Exp $
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

extern void NGCALLF(ispltf,ISPLTF)(float*,float*,int*);

void c_ispltf
#ifdef NeedFuncProto
(
    float rxn,
    float ryn,
    int ient
)
#else
(rxn,ryn,ient)
    float rxn;
    float ryn;
    int ient;
#endif
{
    NGCALLF(ispltf,ISPLTF)(&rxn,&ryn,&ient);
}
