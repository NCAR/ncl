/*
 *	$Id: c_ngdots.c,v 1.1 1997-04-11 17:43:37 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ngdots
#ifdef NeedFuncProto
(
    float *x,
    float *y,
    int num,
    float size,
    int icolor
)
#else
(x,y,num,size,icolor)
    float *x;
    float *y;
    int num;
    float size;
    int icolor;
#endif
{
    float size2;
    size2 = size;
    NGCALLF(ngdots,NGDOTS)(x,y,&num,&size2,&icolor);
}
