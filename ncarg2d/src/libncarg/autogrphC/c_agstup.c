/*
 *	$Id: c_agstup.c,v 1.1 1997-04-11 17:40:39 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_agstup
#ifdef NeedFuncProto
(
    float *xdra,
    int nvix,
    int iivx,
    int nevx,
    int iiex,
    float *ydra,
    int nviy,
    int iivy,
    int nevy,
    int iiey
)
#else
(xdra,nvix,iivx,nevx,iiex,ydra,nviy,iivy,nevy,iiey)
    float *xdra;
    int nvix;
    int iivx;
    int nevx;
    int iiex;
    float *ydra;
    int nviy;
    int iivy;
    int nevy;
    int iiey;
#endif
{
    NGCALLF(agstup,AGSTUP)(xdra,&nvix,&iivx,&nevx,&iiex,ydra,&nviy,&iivy,&nevy,&iiey);
}
