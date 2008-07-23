/*
 *	$Id: c_dashdc.c,v 1.5 2008-07-23 16:16:45 haley Exp $
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

extern void NGCALLF(dashdc,DASHDC)(NGstring,int*,int*,int);

void c_dashdc
#ifdef NeedFuncProto
(
    char *ipat,
    int jcrt,
    int jsize
)
#else
(ipat,jcrt,jsize)
    char *ipat;
    int jcrt;
    int jsize;
#endif
{
    NGstring ipat2;
    int len;

    len = NGSTRLEN(ipat);
    ipat2 = NGCstrToFstr(ipat,len);
    NGCALLF(dashdc,DASHDC)(ipat2,&jcrt,&jsize,len);
}
