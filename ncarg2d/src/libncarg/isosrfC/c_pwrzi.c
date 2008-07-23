/*
 *	$Id: c_pwrzi.c,v 1.5 2008-07-23 16:16:56 haley Exp $
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

extern void NGCALLF(pwrzi,PWRZI)(float*,float*,float*,NGstring,int*,int*,
                                 int*,int*,int*,int);

void c_pwrzi 
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
    NGCALLF(pwrzi,PWRZI)(&x,&y,&z,id2,&n,&isize,&lin3,&itop,&icnt,len);
}
