#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WKID   1

/*
 *  This program illustrates appending to metafiles, both to
 *  metafiles that have been suspended in the middle of a picture
 *  as well as to previously created full metafiles.  The logic
 *  of the code is:
 *
 *         o  Open and activate an initial metafile and establish
 *            values for the primitive attributes for that file.
 *         o  Draw a line and a filled area in the first metafile
 *            and suspend output.
 *         o  Open a second metafile, establish values for the
 *            primitive attributes in that metafile.
 *         o  Draw a picture in the second metafile and deactivate
 *            it and close it.
 *         o  Reopen the first metafile and complete drawing the
 *            first picture.
 *         o  Close the first metafile.
 *         o  Reopen the second metafile and draw another picture.
 *         o  Close the second metafile.
 *
 *  Two metafiles are created.  The first, gmeta1, has one picture;
 *  the second, gmeta2, has two pictures.
 *
 *
 *  Number of colors in the color tables.
 */
#define NCOLS  5

main()
{
/*
 *  Arrays for storing the attribute settings, and the color tables. 
 */
	int             ic1[14], ic2[14], i;
	float           rc1[7], rc2[7];
    Gtext_font_prec tfp;
    Gtext_align     text_align;
	Gcolr_rep       ctab1[NCOLS], ctab2[NCOLS];

	extern void drprim();
/*
 *  Define the color tables for the two metafiles.
 */
	ctab1[0].rgb.red = 1.; ctab1[0].rgb.green = 1.; ctab1[0].rgb.blue = 1.;
	ctab1[1].rgb.red = 0.; ctab1[1].rgb.green = 0.; ctab1[1].rgb.blue = 0.;
	ctab1[2].rgb.red = 1.; ctab1[2].rgb.green = 0.; ctab1[2].rgb.blue = 0.;
	ctab1[3].rgb.red = 0.; ctab1[3].rgb.green = 1.; ctab1[3].rgb.blue = 0.;
	ctab1[4].rgb.red = 0.; ctab1[4].rgb.green = 0.; ctab1[4].rgb.blue = 1.;

	ctab2[0].rgb.red = 0.; ctab2[0].rgb.green = 0.; ctab2[0].rgb.blue = 0.;
	ctab2[1].rgb.red = 1.; ctab2[1].rgb.green = 1.; ctab2[1].rgb.blue = 1.;
	ctab2[2].rgb.red = 0.; ctab2[2].rgb.green = 1.; ctab2[2].rgb.blue = 1.;
	ctab2[3].rgb.red = 1.; ctab2[3].rgb.green = 0.; ctab2[3].rgb.blue = 1.;
	ctab2[4].rgb.red = 1.; ctab2[4].rgb.green = 1.; ctab2[4].rgb.blue = 0.;
/*
 *  Open GKS.
 */
	gopen_gks("stdout",0);
/*
 *  Open a metafile named "gmeta1" and begin a picture, using
 *  the color table in ctab1.
 */
	c_ngsetc("ME","gmeta1");
    gopen_ws(WKID, NULL, 1);
    gactivate_ws (WKID);
	for( i = 0; i < NCOLS; i++ ) {
        gset_colr_rep(1,i,&ctab1[i]);
	}
/*
 *  Establish values for the GKS attributes for gmeta1.
 */
	gset_linetype(1);
	gset_line_colr_ind(1);
	gset_linewidth(5.);
	gset_fill_int_style(GSTYLE_SOLID);
	gset_fill_colr_ind(4);
	gset_marker_type(2);
	gset_marker_colr_ind(2);
	gset_marker_size(5.);
	gset_text_colr_ind(3);
	gset_char_ht(0.05);
    tfp.font = 12;
    tfp.prec = GPREC_STROKE;
    gset_text_font_prec(&tfp);
    text_align.hor = GHOR_CTR;
    text_align.vert = GVERT_HALF;
    gset_text_align(&text_align);
/*
 *  Draw a line and a filled area.
 */
    drprim(" ",0);
/*
 *  Save the current attribute settings and suspend drawing in gmeta1.
 */
    c_ngsrat(2,ic1,rc1);
	gdeactivate_ws (WKID);
	c_ngmftc(1);
/*
 *  Open and activate a metafile named gmeta2.
 */
	c_ngsetc("ME","gmeta2");
    gopen_ws(WKID, NULL, 1);
    gactivate_ws (WKID);
	for( i = 0; i < NCOLS; i++ ) {
        gset_colr_rep(1,i,&ctab2[i]);
	}
/*
 *  Draw a picture and close the workstation.
 */
	gset_linetype(2);
	gset_line_colr_ind(4);
	gset_linewidth(1.);
	gset_fill_int_style(GSTYLE_HATCH);
	gset_fill_style_ind(5);
	gset_fill_colr_ind(1);
	gset_marker_type(4);
	gset_marker_colr_ind(2);
	gset_marker_size(10.);
	gset_text_colr_ind(3);
	gset_char_ht(0.03);
	tfp.font = 13;
    tfp.prec = GPREC_STROKE;
    gset_text_font_prec(&tfp);
    text_align.hor = GHOR_CTR;
    text_align.vert = GVERT_HALF;
    gset_text_align(&text_align);
	drprim(" ",0);
	drprim("gmeta2 - picture 1",1);
	c_frame();
/*
 *  Save the attribrutes of the second metafile.
 */
	c_ngsrat(2,ic2,rc2);
/*
 *  Close the workstation for gmeta2.
 */
	gdeactivate_ws (WKID);
	gclose_ws (WKID);
/*
 *  Reopen gmeta1 and add to the first picture.
 */
	c_ngreop(1, 2, 1, "gmeta1", 2, ic1, rc1, NCOLS, 0, ctab1);
    gactivate_ws (WKID);

	drprim("gmeta1",1);
	c_frame();
/*
 *  Deactivate and close the first metafile.
 */
	gdeactivate_ws (WKID);
	gclose_ws (WKID);
/*
 *  Reopen and add a second picture to gmeta2.
 */
	c_ngreop(1, 2, 1, "gmeta2", 2, ic2, rc2, NCOLS, 0, ctab2);
    gactivate_ws (WKID);
	drprim(" ",0);
	drprim("gmeta2 - picture 2",1);
	c_frame();
/*
 *  Close things down.
 */
	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();

}

void drprim(char *str,int iopt)
{
/*
 *  Draws output primitives.
 *
 *   gpolyine and gfill_area if iopt=0;
 *   gpolymarker and gtext if iopt=1.
 */
    Gpoint_list line;
    Gpoint_list area;
    Gpoint_list marker;
	Gpoint pos;

	pos.x = 0.3;
	pos.y = 0.5;

	line.num_points = 2;
    line.points = (Gpoint *)malloc(line.num_points*sizeof(Gpoint));
    if( !line.points ) {
        fprintf( stderr, "drprim: Not enough memory to create line structure\n" );
        gemergency_close_gks();
        exit(1);
    }
    line.points[0].x = 0.15; line.points[0].y = 0.7;
    line.points[1].x = 0.45; line.points[1].y = 0.7;

	area.num_points = 5;
    area.points = (Gpoint *)malloc(area.num_points*sizeof(Gpoint));
    if( !area.points ) {
        fprintf( stderr, "drprim: Not enough memory to create area structure\n" );
        gemergency_close_gks();
        exit(1);
    }
    area.points[0].x = 0.6; area.points[0].y = 0.4;
    area.points[1].x = 0.8; area.points[1].y = 0.4;
    area.points[2].x = 0.8; area.points[2].y = 0.8;
    area.points[3].x = 0.6; area.points[3].y = 0.8;
    area.points[4].x = 0.6; area.points[4].y = 0.4;

	marker.num_points = 4;
    marker.points = (Gpoint *)malloc(marker.num_points*sizeof(Gpoint));
    if( !marker.points ) {
        fprintf( stderr, "drprim: Not enough memory to create marker structure\n" );
        gemergency_close_gks();
        exit(1);
    }
    marker.points[0].x = 0.2; marker.points[0].y = 0.2;
    marker.points[1].x = 0.4; marker.points[1].y = 0.2;
    marker.points[2].x = 0.6; marker.points[2].y = 0.2;
    marker.points[3].x = 0.8; marker.points[3].y = 0.2;

	if (iopt == 0) {
        gpolyline(&line);
        gfill_area(&area);
	}
	else if (iopt == 1)  {
        gpolymarker(&marker);
        gtext(&pos,str);
        c_ngdots(&pos.x,&pos.y,1,0.01,1);
	}
	
	return;
}

