/*
 *	$Id: c_wmlgnd.c,v 1.1 1997-04-11 17:45:29 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_wmlgnd
#ifdef NeedFuncProto
(
    float x,
    float y,
    int ntype,
    int irows,
    int icols
)
#else
(x,y,ntype,irows,icols)
    float x;
    float y;
    int ntype;
    int irows;
    int icols;
#endif
{
    float x2, y2;
    x2 = x;
    y2 = y;
    NGCALLF(wmlgnd,WMLGND)(&x2,&y2,&ntype,&irows,&icols);
}
