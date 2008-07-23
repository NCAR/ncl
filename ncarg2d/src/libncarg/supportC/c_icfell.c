/*
 *	$Id: c_icfell.c,v 1.5 2008-07-23 16:17:04 haley Exp $
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

int c_icfell
#ifdef NeedFuncProto
(
    char *messg,
    int nerrf
)
#else
(messg,nerrf)
    char *messg;
    int nerrf;
#endif
{
    NGstring messg2;
    extern int NGCALLF(icfell,ICFELL)(NGstring,int*,int);
    int len;
    len = NGSTRLEN(messg);
    messg2 = NGCstrToFstr(messg,len);
    return(NGCALLF(icfell,ICFELL)(messg2,&nerrf,len));
}
