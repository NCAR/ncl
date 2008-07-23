/*
 *	$Id: c_wmlabc.c,v 1.5 2008-07-23 16:17:09 haley Exp $
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

extern void NGCALLF(wmlabc,WMLABC)(float*,float*,NGstring,NGstring,int,int);

void c_wmlabc
#ifdef NeedFuncProto
(
    float x,
    float y,
    char *city,
    char *temps
)
#else
(x,y,city,temps)
    float x;
    float y;
    char *city;
    char *temps;
)
#endif
{
    NGstring city2;
    NGstring temps2;
	int len1, len2;
	len1 = NGSTRLEN(city);
	len2 = NGSTRLEN(temps);
	city2 = NGCstrToFstr(city,len1);
	temps2 = NGCstrToFstr(temps,len2);
    NGCALLF(wmlabc,WMLABC)(&x,&y,city2,temps2,len1,len2);
}
