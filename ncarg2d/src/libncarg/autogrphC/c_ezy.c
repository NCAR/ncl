/*
 *	$Id: c_ezy.c,v 1.1 1997-04-11 17:40:43 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ezy
#ifdef NeedFuncProto
(
    float *ydra,
    int npts,
    char *labg
)
#else
(ydra,npts,labg)
    float *ydra;
    int npts;
    char *labg;
#endif
{
    NGstring labg2;
    int len;

    len = NGSTRLEN(labg);
    labg2 = NGCstrToFstr(labg,len);
    NGCALLF(ezy,EZY)(ydra,&npts,labg2,len);
}
