#include <ncarg/ncargC.h>

void c_tdez2d
#ifdef NeedFuncProto
(
    int nx,
    int ny,
    float *x,
    float *y,
    float *z,
    float rmult,
    float theta,
    float phi,
    int ist
)
#else
 (nx,ny,x,y,z,rmult,theta,phi,ist)
    int nx,
    int ny,
    float *x,
    float *y,
    float *z,
    float rmult,
    float theta,
    float phi,
    int ist
#endif
{
    int nx2;
    int ny2;
    float rmult2;
    float theta2;
    float phi2;
    int ist2;

    nx2 = nx;
    ny2 = ny;
    rmult2 = rmult;
    theta2 = theta;
    phi2 = phi;
    ist2 = ist;

    NGCALLF(tdez2d,TDEZ2D)(&nx2,&ny2,x,y,z,&rmult2,&theta2,&phi2,&ist2);
}
