/*
 *	$Id: c_halfax.c,v 1.1 1997-04-11 17:43:09 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_halfax
#ifdef NeedFuncProto
(
    int mjrx,
    int mnrx,
    int mjry,
    int mnry,
    float xint,
    float yint,
    int ixlb,
    int iylb
)
#else
(mjrx,mnrx,mjry,mnry,xint,yint,ixlb,iylb)
    int mjrx;
    int mnrx;
    int mjry;
    int mnry;
    float xint;
    float yint;
    int ixlb;
    int iylb;
#endif
{
    float xint2,yint2;
    xint2 = xint;
    yint2 = yint;
    NGCALLF(halfax,HALFAX)(&mjrx,&mnrx,&mjry,&mnry,&xint2,&yint2,&ixlb,&iylb);
}
