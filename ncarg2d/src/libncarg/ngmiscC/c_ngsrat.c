/*
 *	$Id: c_ngsrat.c,v 1.1 1997-04-11 17:43:44 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ngsrat
#ifdef NeedFuncProto
(
    int iopt,
    int *iat,
    float *rat
)
#else
(iopt,iat,rat)
    int iopt;
    int *iat;
    float *rat;
#endif
{
    NGCALLF(ngsrat,NGSRAT)(&iopt,iat,rat);
}
