/*
 *	$Id: c_ntst08.c,v 1.2 1994-06-21 15:00:34 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define NX     200
#define NY     200
#define NITER  101

typedef struct {
    float x;
    float y;
} complex;

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
/*
 *  Produce an NX x NY  CELL ARRAY based on the Mandelbrot set--color
 *  the cells depending upon the speed of convergence or divergence.
 */
    int i, j, k, l, iter;
    Gint colr_ind, err_ind;
    Gclip clip_ind;
    float xl, xr, yb, yt, dx, dy, x, y, h;
    Glimit win_limits, vp_limits;
    Gcolr_rep rgb;
    Grect rect;
    Gpat_rep colia;
    complex z;
/*
 *  region of interest.
 */
    xl = -2.;
    xr = 0.5;
    yb = -1.25;
    yt = 1.25;
/*
 *  open gks, open and activate the metafile workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws( WKID, NULL, WSTYPE);
    gactivate_ws( WKID );
/*
 *  set up normalizaton transformation.
 */
    win_limits.x_min = xl;
    win_limits.x_max = xr;
    win_limits.y_min = yb;
    win_limits.y_max = yt;
    gset_win(1,&win_limits);
    vp_limits.x_min = .1;
    vp_limits.x_max = .9;
    vp_limits.y_min = 0.;
    vp_limits.y_max = .8;
    gset_vp(1,&vp_limits);
    gsel_norm_tran(1);
/*
 *  define color indices in a continuous spectrum.
 */
    for( k = 1; k <= NITER; k++ ) {
        h = (float)k/(float)(NITER)*360.;;
        c_hlsrgb(h,50.,100.,&rgb.rgb.red,&rgb.rgb.green,&rgb.rgb.blue);
        gset_colr_rep(WKID,k,&rgb);
    }
    rgb.rgb.red = 1.;
    rgb.rgb.green = rgb.rgb.blue = 0.;
    gset_colr_rep(WKID,NITER+1,&rgb);
/* 
 * Set up cell array structure
 */
    colia.colr_array = (Gint *)malloc(NX*NY*sizeof(Gint));
    colia.dims.size_x = NX;
    colia.dims.size_y = NY;
    if( colia.colr_array != NULL ) {
        l = 0;
/*
 *  set up the cell array and call gfa.
 */
		dx = (xr-xl)/NX;
		dy = (yt-yb)/NY;
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
/*
 * Draw the cell array.
 */
		rect.p.x = xl;
		rect.p.y = yb;
		rect.q.x = xr;
		rect.q.y = yt;
		gcell_array (&rect,&colia);
	}
    else {
        fprintf( stderr, "c_tst7: no memory for cell array\n" );
        exit(1);
    }
/*
 *  label the plot (plotchar strokes its characters with lines, so the
 *  plotchar character attributes are controlled by the gks polyline 
 *  attributes).
 */
    gset_line_colr_ind(NITER+1);
    c_pcseti("CD",1);
    gset_clip_ind(GIND_NO_CLIP);
    c_plchhq(xl+.5*(xr-xl),yb+(yt-yb)*1.05,"Cell Array",.021,0.,0.);
    c_frame();
/*
 * Test ginq_line_colr_ind
 */
    ginq_line_colr_ind(&err_ind,&colr_ind);
    if( colr_ind != NITER+1 ) {
        printf( "ntst08:  GINQ_LINE_COLR_IND UNSUCCESSFUL\n" );
	}
	else {
        printf( "ntst08:  GINQ_LINE_COLR_IND SUCCESSFUL\n" );
	}
/*
 * Test ginq_clip
 */
    ginq_clip(&err_ind,&clip_ind);
    printf( "clip_ind should be 0, clip_ind is really %d\n", clip_ind );
/*
 *  deactivate and close the workstation, close gks.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

convg(z,num,tolsm,tollg,iter)
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
