/*
 *	$Id: c_tick43.c,v 1.1 1997-04-11 17:45:11 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_tick43 
#ifdef NeedFuncProto
(
    int magu,
    int minu,
    int magv,
    int minv,
    int magw,
    int minw
)
#else
 (magu,minu,magv,minv,magw,minw)
    int magu;
    int minu;
    int magv;
    int minv;
    int magw;
    int minw;
#endif
{
    NGCALLF(tick43,TICK43)(&magu,&minu,&magv,&minv,&magw,&minw);
}
