/*
 *	$Id: c_perim3.c,v 1.1 1997-04-11 17:45:09 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_perim3 
#ifdef NeedFuncProto
(
    int magr1,
    int mini1,
    int magr2,
    int mini2,
    int iwhich,
    float var
)
#else
 (magr1,mini1,magr2,mini2,iwhich,var)
    int magr1;
    int mini1;
    int magr2;
    int mini2;
    int iwhich;
    float var;
#endif
{
    float var2;
    var2 = var;
    NGCALLF(perim3,PERIM3)(&magr1,&mini1,&magr2,&mini2,&iwhich,&var2);
}
