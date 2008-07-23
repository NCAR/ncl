/*
 *	$Id: c_nglogo.c,v 1.2 2008-07-23 16:16:58 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2002                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <ncarg/ncargC.h>

extern void NGCALLF(nglogo,NGLOGO)(int*, float*, float*, float*, int*, int*, int*);

void c_nglogo
#ifdef NeedFuncProto
(
    int iwk,
    float x,
    float y,
    float size,
    int itype,
    int icol1,
    int icol2
)
#else
(iwk,x,y,size,itype,icol1,icol2)
    int iwk;
    float x;
    float y;
    float size;
    int itype;
    int icol1;
    int icol2;
#endif
{
    NGCALLF(nglogo,NGLOGO)(&iwk,&x,&y,&size,&itype,&icol1,&icol2);
}
