/*
 *	$Id: c_wmlabt.c,v 1.1 1997-04-11 17:45:28 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_wmlabt
#ifdef NeedFuncProto
(
    float x,
    float y,
    char *label,
    int iflg
)
#else
(x,y,label,iflg)
    float x;
    float y;
    char *label;
    int iflg;
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
    NGCALLF(wmlabt,WMLABT)(&x2,&y2,label2,&iflg,len);
}
