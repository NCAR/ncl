/*
 *      $Id: c_tdez3d.c,v 1.2 1998-12-02 01:27:44 fred Exp $
 */
#include <ncarg/ncargC.h>

void c_tdez3d
#ifdef NeedFuncProto
(
    int nx,
    int ny,
    int nz,
    float *x,
    float *y,
    float *z,
    float *u,
    float value,
    float rmult,
    float theta,
    float phi,
    int ist
)
#else
 (nx,ny,x,y,z,rmult,theta,phi,ist)
    int nx,
    int ny,
    int nz,
    float *x,
    float *y,
    float *z,
    float *u,
    float value,
    float rmult,
    float theta,
    float phi,
    int ist
#endif
{
    int nx2;
    int ny2;
    int nz2;
    float value2;
    float rmult2;
    float theta2;
    float phi2;
    int ist2;

    nx2 = nx;
    ny2 = ny;
    nz2 = nz;
    value2 = value;
    rmult2 = rmult;
    theta2 = theta;
    phi2 = phi;
    ist2 = ist;
    
    NGCALLF(tdez3d,TDEZ3D)(&nx2,&ny2,&nz2,x,y,z,u,&value2,&rmult2,
            &theta2,&phi2,&ist2);
}
