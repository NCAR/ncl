/*
 *	$Id: c_wmlabc.c,v 1.1 1997-04-11 17:45:27 haley Exp $
 */
#include <ncarg/ncargC.h>

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
    float x2, y2;
    x2 = x;
    y2 = y;
	len1 = NGSTRLEN(city);
	len2 = NGSTRLEN(temps);
	city2 = NGCstrToFstr(city,len1);
	temps2 = NGCstrToFstr(temps,len2);
    NGCALLF(wmlabc,WMLABC)(&x2,&y2,city2,temps2,len1,len2);
}
