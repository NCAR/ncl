#include <math.h>
#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define max(x,y) ((x) > (y) ? (x) : (y))

#define WSTYPE SED_WSTYPE
#define WKID   1

#define IWTYPE   1
#define ILD    121

main()
{
/*
 *  Illustrate clipping.
 *
 *
 *  Define error file, Fortran unit number, and workstation type,
 *  and workstation ID.
 */
	int i, k, j;
	float scale;
	Gcolr_rep rgb;
	float plx[ILD],ply[ILD];
	float xc[2],yc[2],cx[2][2],cy[2][2];
	float radc = .0174532;
	Gpoint_list line;
    Glimit win_limits, vp_limits;
/*
 *  Center positions for spirals.
 */
	xc[0] = .31007; xc[1] = .67052;
	yc[0] = .62000; yc[1] = .20000;
/*
 *  Clipping rectangles.
 */
	cx[0][0] = .20; cx[1][0] = .77; cx[0][1] = .20; cx[1][1] = .77;
	cy[0][0] = .50; cy[1][0] = .76; cy[0][1] = .08; cy[1][1] = .347;
/*
 *
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks("stdout", 0 );
	gopen_ws(WKID, NULL, IWTYPE);
	gactivate_ws(WKID);
/*
 *  Define indices, color index 0 defines the background color.
 */
	rgb.rgb.red = 1.0; rgb.rgb.green =  1.0; rgb.rgb.blue = 1.0;
	gset_colr_rep(WKID,0,&rgb);
	rgb.rgb.red = 0.0; rgb.rgb.green =  0.0; rgb.rgb.blue = 0.0;
	gset_colr_rep(WKID,1,&rgb);
	rgb.rgb.red = 0.4; rgb.rgb.green =  0.0; rgb.rgb.blue = 0.4;
	gset_colr_rep(WKID,2,&rgb);
	rgb.rgb.red = 0.0; rgb.rgb.green =  0.0; rgb.rgb.blue = 1.0;
	gset_colr_rep(WKID,3,&rgb);
/*
 *  Set the line width to 2 times the nominal width.  This setting
 *  may not be honored by all hardware devices.
 */
	gset_linewidth(2.0);
	for( k = 1; k <= 2; k++ ) {
/*
 *  Define the clipping rectangle.
 */
		win_limits.x_min = cx[0][k-1];
		win_limits.x_max = cx[1][k-1];
		win_limits.y_min = cy[0][k-1];
		win_limits.y_max = cy[1][k-1];
		gset_win(1,&win_limits);
		vp_limits.x_min = cx[0][k-1];
		vp_limits.x_max = cx[1][k-1];
		vp_limits.y_min = cy[0][k-1];
		vp_limits.y_max = cy[1][k-1];
		gset_vp(1,&vp_limits);
        gsel_norm_tran(1);
/*
 *  Clipping is on for the second curve only.
 */
        if( k == 1 ) gset_clip_ind(GIND_NO_CLIP);
        else         gset_clip_ind(GIND_CLIP);
/*
 *  Draw a boundary around the clipping rectangle.
 */
		line.num_points = 121;
		line.points = (Gpoint *)malloc(line.num_points*sizeof(Gpoint));
        line.points[0].x = cx[0][k-1];
        line.points[0].y = cy[0][k-1];
        line.points[1].x = cx[1][k-1];
        line.points[1].y = line.points[0].y;
        line.points[2].x = line.points[1].x;
		line.points[2].y = cy[1][k-1];
        line.points[3].x = line.points[0].x;
        line.points[3].y = line.points[2].y;
        line.points[4].x = line.points[0].x;
        line.points[4].y = line.points[0].y;
        gset_line_colr_ind(1);
		line.num_points = 5;
        gpolyline(&line);
/*
 *  Draw the spirals.
 */
        gset_line_colr_ind(3);
        j = 0;
        for( i = 0; i <= 720; i+=6 ) {
			scale = (float)i/4000.;
			j++;
			line.points[j-1].x = xc[0]+scale*cos((float)(i-1)*radc);
			line.points[j-1].y = yc[k-1]+scale*sin((float)(i-1)*radc);
		}
		line.num_points = 121;
        gpolyline(&line);

        j = 0;
        for( i = 2; i <= 722; i+=6 ) {
			scale = (float)i/4000.;
			j++;
			line.points[j-1].x = xc[1]-scale*cos((float)(i-1)*radc);
			line.points[j-1].y = yc[k-1]-scale*sin((float)(i-1)*radc);
		}
        gpolyline(&line);
	}
/*
 *  Turn clipping back off.
 */
        gset_clip_ind(GIND_NO_CLIP);
/*
 *  Label the plot using Plotchar.
 */
	c_pcseti("FN",25);
	c_pcseti("CC",2);
	c_plchhq(.5,.9,"Clipping",.035,0.,0.);
	c_pcseti("FN",21);
	c_pcseti("CC",2);
	c_plchhq(.2,.80,"Clipping off",.022,0.,-1.);
	c_plchhq(.2,.38,"Clipping on" ,.022,0.,-1.);
	c_frame();
/*
 *  Deactivate and close the workstation, close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}
