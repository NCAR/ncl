/*
 *	$Id: c_ezmxy.c,v 1.1 1997-04-11 17:40:41 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ezmxy
#ifdef NeedFuncProto
(
    float *xdra,
    float *ydra,
    int idxy,
    int many,
    int npts,
    char *labg
)
#else
(xdra,ydra,idxy,many,npts,labg)
    float *xdra;
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
    NGCALLF(ezmxy,EZMXY)(xdra,ydra,&idxy,&many,&npts,labg2,len);
}
