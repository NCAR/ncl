#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
    int i, j, nmk;
    float xcp, ycp, yb, yt, xl, xr, rsp, rinc, slp, b1, xinc;
	char lab[3];
	Gcolr_rep rgb[13];
    Gpoint_list line;
	Glimit win_limits, vp_limits;
    Gescape_in_data in_data;
    extern void drbox();
/*
 *  Initialize
 */
	rgb[0].rgb.red = 1.0; rgb[0].rgb.green = 1.0; rgb[0].rgb.blue = 1.0;
	rgb[1].rgb.red = 1.0; rgb[1].rgb.green = 0.0; rgb[1].rgb.blue = 0.0;
	rgb[2].rgb.red = 0.0; rgb[2].rgb.green = 1.0; rgb[2].rgb.blue = 0.0;
	rgb[3].rgb.red = 0.0; rgb[3].rgb.green = 0.0; rgb[3].rgb.blue = 1.0;
	rgb[4].rgb.red = 0.0; rgb[4].rgb.green = 1.0; rgb[4].rgb.blue = 1.0;
	rgb[5].rgb.red = 1.0; rgb[5].rgb.green = 0.0; rgb[5].rgb.blue = 1.0;
	rgb[6].rgb.red = 1.0; rgb[6].rgb.green = 1.0; rgb[6].rgb.blue = 0.0;
	rgb[7].rgb.red = 1.0; rgb[7].rgb.green = 0.5; rgb[7].rgb.blue = 0.0;
	rgb[8].rgb.red = 1.0; rgb[8].rgb.green = 0.0; rgb[8].rgb.blue = 0.5;
	rgb[9].rgb.red = 0.0; rgb[9].rgb.green = 0.5; rgb[9].rgb.blue = 1.0;
	rgb[10].rgb.red = 0.0; rgb[10].rgb.green = 1.0; rgb[10].rgb.blue = 0.5;
	rgb[11].rgb.red = 0.5; rgb[11].rgb.green = 0.0; rgb[11].rgb.blue = 1.0;
	rgb[12].rgb.red = 0.5; rgb[12].rgb.green = 1.0; rgb[12].rgb.blue = 0.0;
    line.num_points = 100;
    line.points = (Gpoint *)malloc(line.num_points*sizeof(Gpoint));
	win_limits.x_min = win_limits.y_min = 0.;
	win_limits.x_max = win_limits.y_max = 1.;
	vp_limits.x_min = vp_limits.y_min = 0.;
	vp_limits.x_max = vp_limits.y_max = 1.;
/*
 *  Open GKS
 */
	gopen_gks("stdout",0);
/*
 *  Open a metafile workstation with name DIFMETA.
 */
    in_data.escape_r1.data = (Gdata *)malloc(80*sizeof(char));
	strcpy( in_data.escape_r1.data, "DIFMETA" );
    in_data.escape_r1.size = 7;
    
	gescape(-1391,&in_data,NULL,NULL);
	gopen_ws(WKID,NULL,WSTYPE);
	gactivate_ws(WKID);
/*
 *  Open WISS
 */
	gopen_ws(2,NULL,3);
/*
 * Create color bars in segments, using gpolyline.
 */
	line.num_points = 2;
	for( i = 1; i <= 9; i++ ) {
        c_gflas1(i);
        gset_colr_rep(WKID,i,&rgb[i-1]);
        sprintf( lab, "%d", i );
        xcp = .1*(float)(i);
        ycp = .89;
        gset_text_colr_ind(i);
        gset_line_colr_ind(i);
        gset_win(1,&win_limits);
        gset_vp(1,&vp_limits);
        gsel_norm_tran(1);
        c_wtstr(xcp,ycp,lab,2,0,0);
        yb = .09*(float)(i-1);
        yt = yb+.08;
        xl = 0.;
        xr = .8;
        rsp = .8/9.;
        gset_win(1,&win_limits);
		vp_limits.x_min = xl;
		vp_limits.x_max = xl+rsp*(float)(10-i);
		vp_limits.y_max = yt;
		vp_limits.y_min = yb;
        gset_vp(1,&vp_limits);
        gsel_norm_tran(1);
        line.points[0].x = 0.;
        line.points[1].x = 1.;
        rinc = .005;
        for( j=1; j <= 200; j++ ) {
			line.points[1].y = (float)j*rinc;
			line.points[0].y = line.points[1].y;
			gpolyline(&line);
		}
        c_gflas2();
	}
/*
 *  Define color indices in the metafile.
 */
	for( i = 10; i <= 13; i++ ) {
		gset_colr_rep(WKID,i,&rgb[i-1]);
	}
/*
 *  FILL AREA in flash buffer.
 */
	c_gflas1(10);
	gset_win(1,&win_limits);
	vp_limits.x_min = vp_limits.y_min = 0.;
	vp_limits.x_max = vp_limits.y_max = 1.;
	gset_vp(1,&vp_limits);
	gsel_norm_tran(1);
	for( i = 1; i <= 9; i++ ) {
        xr = 1.;
        xl = xr-rsp*(float)(i);
        yb = .09*(float)(i-1);
        yt = yb+.08;
        line.points[0].x = xl;
        line.points[0].y = yb;
        line.points[1].x = xr;
        line.points[1].y = yb;
        line.points[2].x = xr;
        line.points[2].y = yt;
        line.points[3].x = xl;
        line.points[3].y = yt;
        line.num_points = 4;
        gset_fill_colr_ind(10-i);
        gset_fill_int_style (GSTYLE_SOLID);
        gfill_area(&line);
	}
	c_gflas2();
/*
 *  POLYMARKERS in flash buffer.
 */
	slp = -0.64/0.62;
	b1 = .0805-slp*.815;
	c_gflas1(11);
	gset_win(1,&win_limits);
	gset_vp(1,&vp_limits);
	gsel_norm_tran(1);
	nmk = 51;
	gset_marker_colr_ind(7);
	gset_marker_type(3);
	for( i=1; i <= nmk; i++ ) {
        xinc = .8/(float)(nmk-1);
        line.points[i-1].x = .10+xinc*(float)(i-1);
        line.points[i-1].y = slp*line.points[i-1].x+b1;
	}
    line.num_points = nmk-2;
	gpolymarker(&line);
	c_gflas2();
/*
 *  Produce plot.
 */
	gset_win(1,&vp_limits);
	gset_vp(1,&win_limits);
	gsel_norm_tran(1);
	gset_line_colr_ind(6);
	drbox(0.000,1.00,.810,.970);
	c_gflas3(1);
	drbox(0.005,.995,.815,.965);
	c_gflas3(2);
	drbox(0.010,.990,.820,.960);
	c_gflas3(3);
	drbox(0.015,.985,.825,.955);
	c_gflas3(4);
	drbox(0.020,.980,.830,.950);
	c_gflas3(5);
	drbox(0.025,.975,.835,.945);
	c_gflas3(6);
	drbox(0.030,.970,.840,.940);
	c_gflas3(7);
	drbox(0.035,.965,.845,.935);
	c_gflas3(8);
	drbox(0.040,.960,.850,.930);
	c_gflas3(9);
	drbox(0.045,.955,.855,.925);
	c_gflas3(10);
	drbox(0.050,.950,.860,.920);
	c_gflas3(11);
	drbox(0.055,.945,.865,.915);
	c_frame();
	c_gflas4(20,"GFLX0");
	c_gflas3(20);
	c_frame();
/*
 *  Close things out
 */
	gclose_ws(2);
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void drbox(xl,xr,yb,yt)
float xl,xr,yb,yt;
{
/*
 *  Draw box.
 */
	float x[5], y[5];
	Gpoint_list line;
	line.num_points = 5;
	line.points = (Gpoint *)malloc(line.num_points*sizeof(Gpoint));
	line.points[0].x = xl;
	line.points[0].y = yb;
	line.points[1].x = xr;
	line.points[1].y = yb;
	line.points[2].x = xr;
	line.points[2].y = yt;
	line.points[3].x = xl;
	line.points[3].y = yt;
	line.points[4].x = xl;
	line.points[4].y = yb;
	gpolyline(&line);
}
