/*
 *	$Id: c_ezmy.c,v 1.1 1997-04-11 17:40:42 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ezmy
#ifdef NeedFuncProto
(
    float *ydra,
    int idxy,
    int many,
    int npts,
    char *labg
)
#else
(ydra,idxy,many,npts,labg)
    float *ydra;
    int idxy;
    int many;
    int npts;
    char *labg;
#endif
{
    NGstring labg2;
    int len;

    len = NGSTRLEN(labg);
    labg2 = NGCstrToFstr(labg,len);
    NGCALLF(ezmy,EZMY)(ydra,&idxy,&many,&npts,labg2,len);
}
