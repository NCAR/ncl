/*
 *	$Id: c_wmlabw.c,v 1.1 1997-04-11 17:45:28 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_wmlabw
#ifdef NeedFuncProto
(
    float x,
    float y,
    char *label
)
#else
(x,y,label)
    float x;
    float y;
    char *label;
)
#endif
{
    NGstring label2;
	int len;
    float x2, y2;
    x2 = x;
    y2 = y;
	len = NGSTRLEN(label);
	label2 = NGCstrToFstr(label,len);
    NGCALLF(wmlabw,WMLABW)(&x2,&y2,label2,len);
}
