#include <math.h>
#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>


#define WSTYPE SED_WSTYPE
#define WKID   1

#define IWTYPE   1

/*
 *  Define two triangles.
 */
float xt1[3] = {10.0, 90.0, 50.0};
float yt1[3] = {0.2,  0.2,  0.3};
float xt2[3] = {10.0, 90.0, 50.0};
float yt2[3] = {0.5,  0.5,  0.4};

main()
{
    Glimit win_limits, vp_limits;
	Gcolr_rep rgb;
	Gpoint_list area;
	Gpoint pos;
	Gtext_align text_align;
	int i;
/*
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks("stdout", 0 );
	gopen_ws(WKID, NULL, IWTYPE);
	gactivate_ws(WKID);
/*
 *  Define a normalization transformation and select it.
 */
	win_limits.x_min = 0.;
	win_limits.x_max = 100;
	win_limits.y_min = .1;
	win_limits.y_max = .5;
	gset_win(1,&win_limits);
	vp_limits.x_min = .05;
	vp_limits.x_max = .95;
	vp_limits.y_min = .05;
	vp_limits.y_max = .95;
	gset_vp(1,&vp_limits);
	gsel_norm_tran(1);
/*
 *  Set up a color table.
 */
	rgb.rgb.red = 1.0; rgb.rgb.green =  1.0; rgb.rgb.blue = 1.0;
	gset_colr_rep(WKID,0,&rgb);
	rgb.rgb.red = 1.0; rgb.rgb.green =  0.0; rgb.rgb.blue = 0.0;
	gset_colr_rep(WKID,1,&rgb);
	rgb.rgb.red = 0.0; rgb.rgb.green =  0.0; rgb.rgb.blue = 1.0;
	gset_colr_rep(WKID,2,&rgb);
	rgb.rgb.red = 0.0; rgb.rgb.green =  0.0; rgb.rgb.blue = 0.0;
	gset_colr_rep(WKID,3,&rgb);
/*
 *  Set fill area interior style to solid.
 */
	gset_fill_int_style(GSTYLE_SOLID);
/*
 *  Fill triangle 1 with red and triangle 2 with blue.
 */
	area.num_points = 3;
	area.points = (Gpoint *)malloc(area.num_points*sizeof(Gpoint));
	gset_fill_colr_ind (1);
	for( i = 0; i < 3; i++ ) {
		area.points[i].x = xt1[i];
		area.points[i].y = yt1[i];
	}
	gfill_area(&area);
	gset_fill_colr_ind (2);
	for( i = 0; i < 3; i++ ) {
		area.points[i].x = xt2[i];
		area.points[i].y = yt2[i];
	}
	gfill_area(&area);
/*
 *  Select normalization transformation 0 for drawing the text
 *  to avoid effects of the non-square aspect ratio of the 
 *  normalizartion transformation on the plotted characters.
 */
	gsel_norm_tran(0);
/*
 *  Set text color to red; align the text as (center, half); 
 *  specify the text size; and draw it.
 */
	gset_text_colr_ind (3);
    text_align.hor = GHOR_CTR;
    text_align.vert = GVERT_HALF;
	gset_text_align(&text_align);
	gset_char_ht (.025);
	pos.x = .5; pos.y = .125;
	gtext(&pos,"Output from a GKS program");
/*
 *  Advance the frame to ensure all output is plotted.
 */
	c_frame();
/*
 *  Deactivate and close the workstation, close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}
