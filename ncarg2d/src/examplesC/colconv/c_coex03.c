/*
 *  $Id: c_coex03.c,v 1.2 1994-06-21 14:59:01 haley Exp $
 */
#include <stdio.h>
#include <math.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define NW    3
#define NCOL 16
#define NSAT  4
#define NPTS  4

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{

/*
 * Create color wheels in HSV space.  Each wheel is produced with
 * a different value of V (the value parameter in HSV space).  Each
 * wheel is composed of 16 wedges.  The value for the hue remains
 * the same within each wedge, and the value for the saturation
 * varies linearly from the center to the outer rim within each wedge.
 * The hues vary from 0. to 360. around the color wheel starting
 * at pure red, and returning to pure red.
 *
 * This program requires PLOTCHAR for drawing its characters.
 */

/*
 *  Define the number of values, the number of hues, the number
 *  of saturations, and the number of points bounding a color box.
 */
    float val[NW],st[NSAT];
    float pi, rinc, hinc, sinc, sat,hue,r,g,b,rlen;
    float theta1, theta2;
    int i, l, il, index, ihue, isat;
    char title[81];
    Gpoint_list fill_area;
    Gcolr_rep color;
    Gasfs iasf;

    iasf.linetype = 1;
    iasf.linewidth = 1;
    iasf.line_colr_ind = 1;
    iasf.marker_type = 1;
    iasf.marker_size = 1;
    iasf.marker_colr_ind = 1;
    iasf.text_font_prec = 1;
    iasf.char_expan = 1;
    iasf.char_space = 1;
    iasf.text_colr_ind = 1;
    iasf.fill_int_style = 1;
    iasf.fill_style_ind = 1;
    iasf.fill_colr_ind = 1;
    pi = 3.14159265;
/*
 *  Define the values to be used.
 */
    val[0] = 0.60;
    val[1] = 0.80;
    val[2] = 1.00;
/*
 *  Y-coordinates for the saturation labels.
 */
    st[0] = -.1;
    st[1] = -.375;
    st[2] = -.625;
    st[3] =  -.850;

	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
    gset_clip_ind (GIND_NO_CLIP);
    gset_asfs(&iasf);
    gset_fill_int_style (GSTYLE_SOLID);
/*
 *  Background and foreground colors.
 */
    color.rgb.red = color.rgb.green = color.rgb.blue = 0.;
    gset_colr_rep(WKID,0,&color);
    color.rgb.red = color.rgb.green = color.rgb.blue = 1.;
    gset_colr_rep(WKID,1,&color);
/*
 * Create structure to pass to gfill_area
 */
    fill_area.num_points = NPTS;
    fill_area.points = (Gpoint *) malloc(2*NPTS*sizeof(Gfloat));
    if( !fill_area.points ) {
        fprintf( stderr, "c_coex03: Not enough memory to create fill area structure\n" );
        gemergency_close_gks();
        exit(1);
    }
/*
 *  Loop on the values.
 */
    for( il = 0; il < NW; il++ ) {
        index = 2;
        c_set(.1,.9,.1,.9,-1.2,1.2,-1.2,1.2,1);
        rinc = 2 * pi / (float)(NCOL);
        hinc = 360.0  / (float)(NCOL);
        sinc =  1.00  / (float)(NSAT - 1);
/*
 *  Loop on the hues.
 */
        for( ihue = 0; ihue <= (NCOL - 1); ihue++ ) {
            hue    =  (float)ihue * hinc;
            theta1 = ((float)ihue -.5) * rinc;
            theta2 = ((float)ihue +.5) * rinc;
            fill_area.points[0].x = 0.0;
            fill_area.points[3].x = 0.0;
            fill_area.points[0].y = 0.0;
            fill_area.points[3].y = 0.0;
/*
 *  Loop on the saturations
 */
            for( isat = 1; isat <= NSAT; isat++ ) {
                sat = ((float)(isat - 1) * sinc);
                c_hsvrgb(hue,sat,val[il],&r,&g,&b);
                color.rgb.red = r;
                color.rgb.green = g;
                color.rgb.blue = b;
                gset_colr_rep(WKID,index,&color);
                gset_fill_colr_ind(index);
                rlen = (float)(isat) / (float)(NSAT);
                fill_area.points[1].x = cos(theta1) * rlen;
                fill_area.points[1].y = sin(theta1) * rlen;
                fill_area.points[2].x = cos(theta2) * rlen;
                fill_area.points[2].y = sin(theta2) * rlen;
/*
 * Fill area
 */
                gfill_area(&fill_area);
                fill_area.points[0].x = fill_area.points[1].x;
                fill_area.points[3].x = fill_area.points[2].x;
                fill_area.points[0].y = fill_area.points[1].y;
                fill_area.points[3].y = fill_area.points[2].y;
                index = index+1;
            }
        }
/*
 *  Label the plots.
 */
        c_pcseti("QU  - QUALITY FLAG",0);
        c_pcseti("CD  - SELECT DUPLEX DATA SET",1);
        sprintf( title,"VALUE = %4.2f", val[il]);
        gset_line_colr_ind(1);
        c_plchhq(0.0,1.25,title,21.0,0.0,0.0);
        for( l=1; l < NSAT; l++ ) {
            sat = ((float)(l)*sinc);
            sprintf( title, "S=%4.2f", sat );
            c_plchhq(0.0,st[l],title,15.0,0.0,0.0);
        }
        c_plotif(0.,0.,2);
        gset_line_colr_ind(1);
        c_line(.98,0.,1.03,0.);
        c_plchhq(1.08,0.,"Hue=0.",15.0,0.0,-1.0);
        c_line( .700,.700, .750,.740);
        c_plchhq( .80,.740,"Hue=45.",15.0,0.0,-1.0);
        c_line(-.700,.700,-.750,.740);
        c_plchhq(-.80,.740,"Hue=135.",15.0,0.0,1.0);
        c_line(-.700,-.700,-.750,-.740);
        c_plchhq(-.80,-.740,"Hue=225.",15.0,0.0,1.0);
        c_line( .700,-.700, .750,-.740);
        c_plchhq( .80,-.740,"Hue=315.",15.0,0.0,-1.0);
        c_frame();
    }
    free(fill_area.points);
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}





