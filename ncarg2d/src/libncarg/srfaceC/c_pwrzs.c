/*
 *	$Id: c_pwrzs.c,v 1.5 2008-07-23 16:17:02 haley Exp $
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

extern void NGCALLF(pwrzs,PWRZS)(float*,float*,float*,NGstring,int*,int*,
                                 int*,int*,int*,int);

void c_pwrzs
#ifdef NeedFuncProto
(
    float x,
    float y,
    float z,
    char *id,
    int n,
    int isize,
    int lin3,
    int itop,
    int icnt
)
#else
(x,y,z,id,n,isize,lin3,itop,icnt)
    float x;
    float y;
    float z;
    char *id;
    int n;
    int isize;
    int lin3;
    int itop;
    int icnt;
#endif
{
    NGstring id2;
    int len;
    len = NGSTRLEN(id);
    id2 = NGCstrToFstr(id,len);
    NGCALLF(pwrzs,PWRZS)(&x,&y,&z,id2,&n,&isize,&lin3,&itop,&icnt,len);
}
