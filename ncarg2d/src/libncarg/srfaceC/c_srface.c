/*
 *	$Id: c_srface.c,v 1.1 1997-04-11 17:44:34 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_srface
#ifdef NeedFuncProto
(
    float *x,
    float *y,
    float *z,
    int *m,
    int mx,
    int nx,
    int ny,
    float s[6],
    float stereo
)
#else
(x,y,z,m,mx,nx,ny,s,stereo)
    float *x;
    float *y;
    float *z;
    int *m;
    int mx;
    int nx;
    int ny;
    float s[6];
    float stereo;
#endif
{
    float stereo2;

    stereo2 = stereo;
    NGCALLF(srface,SRFACE)(x,y,z,m,&mx,&nx,&ny,s,&stereo2);
}
