/*
 *  $Id: c_ffex01.c,v 1.1 1994-07-28 20:13:25 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define PI      3.14159
#define MSIZE   21
#define NSIZE   25
#define IWSIZE  2*MSIZE*NSIZE

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	extern void ffex01();
	int ierr, idm;
	float rdm, sdm;
/*
 *  Open GKS, open and activate a workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws(WKID, NULL, WSTYPE);
    gactivate_ws(WKID);
/*
 * Invoke demo driver
 */
	ffex01(&ierr);
/*
 *  Deactivate and close workstation, close GKS.
 */
    gdeactivate_ws (WKID);
    gclose_ws(WKID);
    gclose_gks();
}

void ffex01 (ierror)
int *ierror;
{
    extern double cos();
	int i, j, idm;
    float gisize, gjsize;
	float u[NSIZE][MSIZE], v[NSIZE][MSIZE], wrk[IWSIZE];
	float rdm, sdm;
/*
 * Set the grid dimensions.
 */
	int m = MSIZE, n = NSIZE;
/*
 * Specify horizontal and vertical vector components U and V on
 * the rectangular grid.
 */
	gisize = 2.0*PI/(float)m;
	gjsize = 2.0*PI/(float)n;
	for( j = 0; j < n; j++ ) {
		for( i = 0; i < m; i++ ) {
            u[j][i] = cos(gisize*((float)(i+1)-1.0));
            v[j][i] = cos(gjsize*((float)(j+1)-1.0));
		}
	}
/*
 * Draw the field with streamlines overlaid on vectors
 */
    idm = 0;
    rdm = sdm = 0.;
	c_vvinit((float *)u,m,(float *)v,m,&rdm,idm,m,n,&sdm,idm);
	c_vvectr((float *)u,(float *)v,&rdm,&idm,0,&sdm);
	c_stinit((float *)u,m,(float *)v,m,&rdm,idm,m,n,wrk,IWSIZE);
	c_stream((float *)u,(float *)v,&rdm,&idm,0,wrk);
	c_perim(1,0,1,0);
	c_frame();
/*
 * Draw just the vectors
 */
    idm = 0;
    rdm = sdm = 0.;
	c_vvinit((float *)u,m,(float *)v,m,&rdm,idm,m,n,&sdm,idm);
	c_vvectr((float *)u,(float *)v,&rdm,&idm,0,&sdm);
	c_perim(1,0,1,0);
	c_frame();
/*
 * Draw just the streamlines
 */
    idm = 0;
    rdm = 0.;
	c_stinit((float *)u,m,(float *)v,m,&rdm,idm,m,n,wrk,IWSIZE);
	c_stream((float *)u,(float *)v,&rdm,&idm,0,wrk);
	c_perim(1,0,1,0);
	c_frame();
}
