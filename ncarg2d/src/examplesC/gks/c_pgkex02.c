#include <math.h>
#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>


#define WSTYPE SED_WSTYPE
#define WKID   1

#define IWTYPE   1

float zzx[9] = {-9.0, -8.0, -7.0, -6.0, -5.0, -4.0, -3.0, -2.0, -1.0};
float zzyl[9] = {6.5,  8.5,  6.5,  8.5,  6.5,  8.5,  6.5,  8.5,  6.5};
float zzym[9] = {-1.0,  1.0, -1.0,  1.0, -1.0,  1.0, -1.0,  1.0, -1.0};
float cirx[9] = {6.15, 5.26, 4.25, 3.59, 3.59, 4.25, 5.26, 6.15, 6.50};
float ciry[9] = {8.46, 8.98, 8.80, 8.01, 6.99, 6.20, 6.02, 6.54, 7.50};

main()
{
    int i, j, ix, iy, jx, jy, l;
    Glimit win_limits, vp_limits;
	Gpoint_list line;
/*
 *  Simple demo of GKS output primitives puts out sample 
 *  POLYLINE, POLYMARKER, TEXT, FILL AREA and CELL ARRAY. 
 */
	Gtext_align txal;
	Gpoint pos;
    Gpat_rep icells;
    Grect rect;
/*
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks("stdout", 0 );
	gopen_ws(WKID, NULL, IWTYPE);
	gactivate_ws(WKID);
/*
 * Create structure for lines
 */
	line.num_points = 9;
	line.points = (Gpoint *)malloc(line.num_points*sizeof(Gpoint));
	if( !line.points ) {
		fprintf( stderr, "Not enough memory to create line structure\n" );
		gemergency_close_gks();
		exit(1);
	}
    icells.colr_array = (Gint *)malloc(12*24*sizeof(Gint));
	if( !icells.colr_array ) {
		fprintf( stderr, "Not enough memory to create cell array\n" );
		gemergency_close_gks();
		exit(1);
	}
    icells.dims.size_x = 24;
    icells.dims.size_y = 12;
/* 
 *  Define normalization transformation 1 and select it.
 */
    win_limits.x_min = -10.;
    win_limits.x_max = 10.;
    win_limits.y_min = -10.;
    win_limits.y_max = 10.;
    gset_win(1,&win_limits);
    vp_limits.x_min = .1;
    vp_limits.x_max = .9;
    vp_limits.y_min = .1;
    vp_limits.y_max = .9;
    gset_vp(1,&vp_limits);
	gsel_norm_tran (1) ;
/* 
 *  Draw a zig-zag POLYLINE. 
 */
	for( i = 0; i < 9; i++ ) {
		line.points[i].x = zzx[i];
		line.points[i].y = zzyl[i];
	}
	gpolyline(&line);
/*
 *  Set the marker type to 2 (plus sign) and draw markers.
 */
	gset_marker_type (2);
	for( i = 0; i < 9; i++ ) {
		line.points[i].y = zzym[i];
	}
	gpolymarker(&line);
/*
 *  Set the fill area interior style to 1 (solid fill) and draw a 
 *  solid filled nonagon.
 */
	gset_fill_int_style (GSTYLE_SOLID);
	for( i = 0; i < 9; i++ ) {
		line.points[i].x = cirx[i];
		line.points[i].y = ciry[i];
	}
	gfill_area (&line);
/*
 *  Define 24x12 foreground/background checkerboard pattern. 
 */
	l = 0;
	for( ix = 1; ix <= 24; ix++ ) {
        jx = ix % 2;
        for( iy = 1; iy <= 12; iy++ ) {
			jy = iy % 2;
			if ((jx == 1 && jy == 1) || (jx == 0 && jy == 0)) {
				icells.colr_array[l++] = 1;
            }
            else {
                icells.colr_array[l++] = 0;
            }
        }
    }
/*
 *  Draw the checkerboard with CELL ARRAY. 
 */
	rect.p.x = 1.5;
	rect.p.y = -1.25;
	rect.q.x = 8.5;
	rect.q.y = 1.25;
    gcell_array(&rect,&icells);
/*
 *  Set the character height to 3% of the screen (.03*20.)
 */
	gset_char_ht (0.6) ;
/*
 *  Set the text alignment to "center" in the horizontal and "half" in
 *  the vertical.
 */
	txal.hor = GHOR_CTR;
	txal.vert = GVERT_HALF;
	gset_text_align (&txal);
/*
 *  Draw the text string. 
 */
	pos.x =  0.0;
	pos.y = -7.0;
	gtext (&pos, "Example text string");
/*
 *  Label the primitives.
 */
	pos.x = -5.0; pos.y = 5.0;
	gtext(&pos,"Polyline");
	pos.x = 5.0; pos.y = 5.0;
	gtext(&pos,"Fill area");
	pos.x = -5.0; pos.y = -2.5;
	gtext(&pos,"Polymarker");
	pos.x = 5.0; pos.y = -2.5;
	gtext(&pos,"Cell array");
	pos.x = 0.0; pos.y = -9.5;
	gtext(&pos,"Text");
	c_frame();
/*
 *  Deactive and close the workstation, close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

