/*
 *      $Id: c_slogap.c,v 1.1 1997-04-11 17:44:38 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_slogap
#ifdef NeedFuncProto
(
    float time,
    int mtst
)
#else
(time,mtst)
    float time;
    int mtst;
#endif
{
    NGCALLF(slogap,SLOGAP)(&time,&mtst);
}
