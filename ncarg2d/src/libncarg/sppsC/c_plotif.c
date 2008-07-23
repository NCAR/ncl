/*
 *	$Id: c_plotif.c,v 1.5 2008-07-23 16:17:02 haley Exp $
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

extern void NGCALLF(plotif,PLOTIF)(float*,float*,int*);

void c_plotif
#ifdef NeedFuncProto
(
    float fx,
    float fy,
    int ip
)
#else
(fx,fy,ip)
    float fx;
    float fy;
    int ip;
#endif
{
    NGCALLF(plotif,PLOTIF)(&fx,&fy,&ip);
}
