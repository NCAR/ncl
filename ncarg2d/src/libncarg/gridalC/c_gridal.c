/*
 *	$Id: c_gridal.c,v 1.1 1997-04-11 17:42:53 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_gridal
#ifdef NeedFuncProto
(
    int mjrx,
    int mnrx,
    int mjry,
    int mnry,
    int ixlb,
    int iylb,
    int igph,
    float xint,
    float yint
)
#else
(mjrx,mnrx,mjry,mnry,ixlb,iylb,igph,xint,yint)
    int mjrx;
    int mnrx;
    int mjry;
    int mnry;
    int ixlb;
    int iylb;
    int igph;
    float xint;
    float yint;
#endif
{
    float xint2,yint2;
    xint2 = xint;
    yint2 = yint;
    NGCALLF(gridal,GRIDAL)(&mjrx,&mnrx,&mjry,&mnry,&ixlb,&iylb,&igph,&xint2,&yint2);
}
