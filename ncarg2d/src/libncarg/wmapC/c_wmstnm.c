/*
 *	$Id: c_wmstnm.c,v 1.1 1997-04-11 17:45:32 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_wmstnm
#ifdef NeedFuncProto
(
    float x,
    float y,
    char *imdat
)
#else
(x,y,imdat)
    float x;
    float y;
    char *imdat;
#endif
{
    NGstring imdat2;
	int len1;
    float x2, y2;
    x2 = x;
    y2 = y;
    len1 = NGSTRLEN(imdat);
    imdat2 = NGCstrToFstr(imdat,len1);
    NGCALLF(wmstnm,WMSTNM)(&x2,&y2,imdat2,len1);
}
