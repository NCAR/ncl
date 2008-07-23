/*
 *	$Id: c_seter.c,v 1.5 2008-07-23 16:17:05 haley Exp $
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

extern void NGCALLF(seter,SETER)(NGstring,int*,int*,int);

void c_seter
#ifdef NeedFuncProto
(
    char *messg,
    int nerr,
    int iopt
)
#else
(messg,nerr,iopt)
    char *messg;
    int nerr;
    int iopt;
#endif
{
    NGstring messg2;
    int len;
    len = NGSTRLEN(messg);
    messg2 = NGCstrToFstr(messg,len);
    NGCALLF(seter,SETER)(messg2,&nerr,&iopt,len);
}
