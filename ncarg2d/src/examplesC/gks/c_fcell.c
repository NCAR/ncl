/*
 *	$Id: c_fcell.c,v 1.1 1994-07-27 15:55:08 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

typedef struct {
    float x;
    float y;
} complex;

#define NX  50
#define NY  50
#define NITER 201

main()
{
    Glimit win_limits, vp_limits;
	Gpat_rep colia;
    Grect rect;
	Gcolr_rep rgb;
	extern int convg();
	float h, dx, dy, x, y;
	int k, l, i, j, iter;
/*
 *  Produce an NX x NY  CELL ARRAY based on the Mandelbrot set--color
 *  the cells depending upon the speed of convergence or divergence.
 */
	complex z;
/*
 *  Region of interest.
 */
	float xl = .25, xr = .375, yb = 0., yt = .5;
/*
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
	win_limits.x_min = xl;
	win_limits.x_max = xr;
	win_limits.y_min = yb;
	win_limits.y_max = yt;
	gset_win(1,&win_limits);
	vp_limits.x_min = 0.;
	vp_limits.x_max = 1.;
	vp_limits.y_min = 0.;
	vp_limits.y_max = 1.;
	gset_vp(1,&vp_limits);
	gsel_norm_tran(1);
/* 
 * Set up cell array structure
 */
    colia.colr_array = (Gint *)malloc(NX*NY*sizeof(Gint));
    colia.dims.size_x = NX;
    colia.dims.size_y = NY;
/*
 *  Define color indices in a continuous spectrum.
 */
	for( k = 1; k <= NITER; k++ ) {
        h = (float)(k)/(float)(NITER)*360.;
        c_hlsrgb(h,50.,100.,&rgb.rgb.red,&rgb.rgb.green,&rgb.rgb.blue);
        gset_colr_rep(WKID,k,&rgb);
	}
	rgb.rgb.red = 1.; rgb.rgb.green = rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,NITER+1,&rgb);
/*
 *  Set up the cell array and call GFA.
 */
	dx = (xr-xl)/NX;
	dy = (yt-yb)/NY;
	l = 0;
	for( i = 1; i <= NX; i++ ) {
		x = xl+dx*(float)(i-1);
		for( j = 1; j <= NY; j++ ) {
			y = yb+dy*(float)(j-1);
			z.x = x;
			z.y = y;
			convg(&z,NITER,.001,10000.,&iter);
			colia.colr_array[l++] = iter;
		}
	}
	rect.p.x = xl;
	rect.p.y = yb;
	rect.q.x = xr;
	rect.q.y = yt;
	gcell_array (&rect,&colia);
	c_frame();
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();
}

int convg(z,num,tolsm,tollg,iter)
complex *z;
int num, *iter;
float tolsm, tollg;
{
/*
 *  iterate z(n+1) = z(n)**2+z0 for n=1,...,num .  if the complex absolute
 *  values get smaller than tolsm, then set iter equal to the number of
 *  iterations and return.  if the complex absolute value gets larger than
 *  tollg, set iter to num and return.
 */
    int i;
    float x,y;
    complex zs,zn,zo;
    zs.x = z->x;
    zs.y = z->y;
    zo.x = z->x;
    zo.y = z->y;
    for( i = 1; i <= num; i++ ) {
        zn.x = ((zo.x*zo.x)-(zo.y*zo.y))+zs.x;
        zn.y = (2*zo.x*zo.y)+zs.y;
        x = (zn.x-zo.x)*(zn.x-zo.x);
        y = (zn.y-zo.y)*(zn.y-zo.y);
        if (sqrt(x+y) < tolsm) {
            *iter = i;
            return(1);
        }
        if (sqrt(x+y) > tollg) {
            *iter = i;
            return(1);
        }
        zo.x = zn.x;
        zo.y = zn.y;
    }
    *iter=num;
    return(1);
}
