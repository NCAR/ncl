/*
 *	$Id: c_ppppap.c,v 1.1 1997-04-11 17:43:59 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ppppap
#ifdef NeedFuncProto
(
        float *xcop,
        float *ycop,
        int ncop,
        int nbts
)
#else
(xcop,ycop,ncop,nbts)
        float *xcop;
        float *ycop;
        int ncop;
        int nbts;
)
#endif
{
    NGCALLF(ppppap,PPPPAP)(xcop,ycop,&ncop,&nbts);
}
