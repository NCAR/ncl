/*
 *	$Id: c_wmlabs.c,v 1.1 1997-04-11 17:45:27 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_wmlabs
#ifdef NeedFuncProto
(
    float x,
    float y,
    char *symtyp
)
#else
(x,y,symtyp)
    float x;
    float y;
    char *symtyp;
)
#endif
{
    NGstring symtyp2;
	int len;
    float x2, y2;
    x2 = x;
    y2 = y;
	len = NGSTRLEN(symtyp);
	symtyp2 = NGCstrToFstr(symtyp,len);
    NGCALLF(wmlabs,WMLABS)(&x2,&y2,symtyp2);
}
