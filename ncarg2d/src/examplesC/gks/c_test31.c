#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define PI   3.1415926
#define MXPTS  6001
#define MNPTS    12
#define max(x,y)    ((x) > (y) ? (x) : (y))

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
    Gtran_matrix tran_matrix;
    Gpoint point;
    Gvec shift, scale;
    Gcolr_rep colr_rep;
    Gint_list seg_names;
    Gint length_list, ier;
    int i;    
    extern void bullet();
/*
 * Initialize
 */
    point.x = 0.25;
    point.y = 0.5;
    shift.delta_x = shift.delta_y = 0.25;
    scale.delta_x = 0.5;
    scale.delta_y = 1.0;
	gopen_gks("stdout",0);
/*
 *  Open and activate WISS.
 */
	gopen_ws(2,"1",3);
	gactivate_ws(2);
/*
 *  Create two segments.
 */
	gcreate_seg(9);
	bullet(.25,.5,.4,1);
	gclose_seg();
	gcreate_seg(10);
	bullet(.5,.5,.4,2);
	gclose_seg();
/*
 *  Open and activate a workstation.
 */
	gopen_ws(WKID,"2",WSTYPE);
	gactivate_ws(WKID);
/*
 *  Define the segment transformation matrix for segment 9 (scale
 *  the X axis by .5, no rotation, shift by (.25,.5) ).
 */
	geval_tran_matrix(&point,&shift,0.,&scale,0,tran_matrix);
	gset_seg_tran(9,tran_matrix);
/*
 *  Define the segment transformation matrix for segment 10 (rotate
 *  by -PI/8.
 */
    point.x = 0.;
    point.y = 0.;
    shift.delta_x = shift.delta_y = 0.;
    scale.delta_x = 1.;
    scale.delta_y = 1.;
    geval_tran_matrix(&point,&shift,-PI/8.,&scale,0,tran_matrix);
    gset_seg_tran(10,tran_matrix);
/*
 *  Copy the segments to the X workstation.
 */
    colr_rep.rgb.red = 1.;
    colr_rep.rgb.green = colr_rep.rgb.blue = 0.;
	gset_colr_rep(WKID,1,&colr_rep);
    colr_rep.rgb.green = 1.;
    colr_rep.rgb.red = colr_rep.rgb.blue = 0.;
	gset_colr_rep(WKID,2,&colr_rep);
	gcopy_seg_ws(WKID,9);
	gcopy_seg_ws(WKID,10);
	c_frame();
/*
 *  Deactivate the workstations.
 */
	gdeactivate_ws(WKID);
	gdeactivate_ws(2);
/*
 * Test ginq_set_seg_names
 */
/*
 * Try a value of 5, even though we know there
 * are only 2.
 */
    seg_names.num_ints = 5;
    seg_names.ints = (Gint *)malloc(5*sizeof(Gint));
    ginq_set_seg_names( 5, 0, &ier, &seg_names, &length_list );
    printf( "length_list = %d\n", length_list );
    for( i = 0; i < seg_names.num_ints; i++ ) {
        printf( "seg_name[%d] = %d\n", i, seg_names.ints[i] );
	}
/*
 *  Close the workstations.
 */
	gclose_ws(WKID);
	gclose_ws(2);
/*
 *  Close GKS.
 */
	gclose_gks();

}

void bullet(x,y,size,icolor)
float x, y, size;
int icolor;
{
/*
 *  Draw a filled dot at position (X,Y) at size SIZE with color
 *  given by the color index icolor.
 */
    Gint ntnr, icolro, ier;
    Gfill_int_style istylo;
    Gtran norm_tran;
    Gpoint_list area;
	float scale, pts, ainc, ssize, t;
    int i;

	ginq_cur_norm_tran_num(&ier,&ntnr);
	ginq_norm_tran(ntnr,&ier,&norm_tran);
/*
 *  Scale factor for non-square aspect ratios.
 */
	scale = (norm_tran.win.x_max-norm_tran.win.x_min)/(norm_tran.win.y_max-norm_tran.win.y_min);
/*
 *  Calculate the number of points in the circle.
 */ 
	pts = 610.;
	area.num_points = max((int)pts-1,MNPTS);

	if (area.num_points > MXPTS-1) {
        printf( "bullet: The bullet is larger than the window.\n");
		exit();
	}
    area.points = (Gpoint *)malloc(area.num_points*sizeof(Gpoint));
    if( !area.points ) {
		printf( "bullet:  not enough memory for fill area\n");
		return;
	}
	ainc = 2.*PI/area.num_points;
	ssize = .5*size;
	for( i = 0; i < area.num_points; i++ ) {
        t = (float)i*ainc;
        area.points[i].x = x+scale*ssize*cos(t);
        area.points[i].y = y+ssize*sin(t);
	}
/*
 *  Save the fill color index and interior style.
 */
	ginq_fill_colr_ind(&ier,&icolro);
	ginq_fill_int_style(&ier,&istylo);
/*
 *  Set the fill style to solid and the fill collor to icolor.
 */
	gset_fill_int_style (GSTYLE_SOLID);
	gset_fill_colr_ind(icolor);
/*
 *  Draw the bullit.
 */
	gfill_area(&area);
/*
 *  Restore the fill color and style.
 */
    free(area.points);
	gset_fill_colr_ind(icolro);
	gset_fill_int_style(istylo);
	return;
}
