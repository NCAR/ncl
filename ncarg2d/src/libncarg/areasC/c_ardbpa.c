/*
 *	$Id: c_ardbpa.c,v 1.5 2008-07-23 16:16:39 haley Exp $
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

extern void NGCALLF(ardbpa,ARDBPA)(int*,int*,NGstring,int);

void c_ardbpa
#ifdef NeedFuncProto
(
    int *iam,
    int igi,
    char *lab
)
#else
(iam,igi,lab)
    int *iam;
    int igi;
    char *lab;
#endif
{
	NGstring lab2;
    int len;

    len = NGSTRLEN(lab);
	lab2 = NGCstrToFstr(lab,len);
    NGCALLF(ardbpa,ARDBPA)(iam,&igi,lab2,len);
}
