/*
 *	$Id: c_tst8.c,v 1.1 1994-05-13 14:27:50 haley Exp $
 */
#include <stdio.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define ID 10

main ()
{
/*
 *  Illustrate the various fill area interior styles.
 */
    char label[7];
    int istyle,istndx,icolor;
    int i;
    float x,y,scl,hld1,hld2,hld3;
    Gcolr_rep rgb[7];
/*
 *  Open GKS, open and activate the metafile workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws (1,NULL, 1);
    gactivate_ws (1);
/*
 *  Define the necessary color indices.
 */
    rgb[0].rgb.red = 0.; rgb[0].rgb.green = 0.; rgb[0].rgb.blue = .6;
    rgb[1].rgb.red = 1.; rgb[1].rgb.green = 1.; rgb[1].rgb.blue = 1.;
    rgb[2].rgb.red = 1.; rgb[2].rgb.green = 0.; rgb[2].rgb.blue = 0.;
    rgb[3].rgb.red = 0.; rgb[3].rgb.green = 1.; rgb[3].rgb.blue = 0.;
    rgb[4].rgb.red = 1.; rgb[4].rgb.green = 1.; rgb[4].rgb.blue = 0.;
    rgb[5].rgb.red = 0.; rgb[5].rgb.green = 1.; rgb[5].rgb.blue = 1.;
    rgb[6].rgb.red = 1.; rgb[6].rgb.green = 0.; rgb[6].rgb.blue = 1.;
    for( i = 0; i < 7; i++ ) {
        gset_colr_rep(1,i,&rgb[i]);
    }
/*
 *  Draw a star with interior style hollow (the style index is
 *  a dummy in this call since it is ignored for interior style
 *  hollow).
 */
    istyle = 0;
    istndx = 1;
    icolor = 1;
    hld1 = .35; hld2 = .79; hld3 = .09;
    star(hld1,hld2,hld3,istyle,istndx,icolor);
/*
 *  label the hollow area using PLOTCHAR (GKS line color controlles 
 *  the color of the PLOTCHAR characters).
 */
    gset_line_colr_ind(4);
    c_pcseti("CD",1);
    c_plchhq(.17,.77,"Hollow",.020,0.,-1.);
/*
 *  Draw a star with interior style solid (the style index is
 *  a dummy in this call since it is ignored for interior style
 *  Solid).
 */
    istyle = 1;
    istndx = 1;
    icolor = 1;
    hld1 = .75;hld2 = .79;hld3 = .09;
    star(hld1,hld2,hld3,istyle,istndx,icolor);
/*
 *  Label the solid area.
 */
    c_plchhq(.60,.77,"Solid",.020,0.,-1.);
/*
 *  Draw Stars Witho interior style hatch and with the six standardized
 *  hatch styles:
 *
 *    Style index   Fill pattern
 *    -----------   ------------ 
 *       1          Horizontal lines
 *       2          Vertical lines
 *       3          Positive slope lines
 *       4          Negative slope lines
 *       5          Combined vertical and horizontal lines
 *       6          Combined positive slope and negative slope lines
 */
    for(i=1;i<=6;i++){
        x = .2+.3*((i-1)%3)+.02;
        y = .3*((int)(9-i)/3)-.10;
        scl = .15;
        istyle = 3;
        istndx = i;
        icolor = i;
        star(x,y,scl,istyle,istndx,icolor);
/*
 *  Label the hatched areas.
 */
        c_plchhq(x-.20,y,"Hatch,",.0165,0.,-1.);
        sprintf(label,"index %1d",i);
        c_plchhq(x-.20,y-.030,label,.0165,0.,-1.);
    }
/*
 *  Main plot label.
 */
    gset_linewidth(2.0);
    c_plchhq(.5,.95,"Fill area interior styles",.023,0.,0.);
    c_frame();
/*
 *  Deactivate and close the workstation, close GKS.
 */
    gdeactivate_ws (1);
    gclose_ws (1);
    gclose_gks();
}

star(x,y,scl,istyle,istndx,icolor)
float x, y, scl;
int istyle, istndx, icolor;
{
    int i;
/*
 *  Draw a five-pointed star with interior style ISTYLE, style index
 *  ISTNDX (if applicable), and colored using the color defined by 
 *  color index ICOLOR.
 */ 
    float xs[ID],ys[ID];
    Gpoint_list fill_area;
/*
 * Create structure to pass to gfill_area
 */
	fill_area.num_points = ID;
	fill_area.points = (Gpoint *) malloc(2*ID*sizeof(Gfloat));
	if( !fill_area.points ) {
		fprintf( stderr, "star: Not enough memory to create fill area structure\n" );
		gemergency_close_gks();
		exit(1);
	}
/*
 *  Coordinate data for a five-pointed star.
 */
    xs[0] = 0.00000;
    xs[1] = -0.22451;
    xs[2] = -0.95105;
    xs[3] = -0.36327;
    xs[4] = -0.58780;
    xs[5] = 0.00000;
    xs[6] = 0.58776;
    xs[7] = 0.36327;
    xs[8] = 0.95107;
    xs[9] = 0.22453;

    ys[0] = 1.00000;
    ys[1] = 0.30902;
    ys[2] = 0.30903;
    ys[3] = -0.11803;
    ys[4] = -0.80900;
    ys[5] = -0.38197;
    ys[6] = -0.80903;
    ys[7] = -0.11805;
    ys[8] = 0.30898;
    ys[9] = 0.30901;

    for( i = 0; i < ID; i++ ) {
        fill_area.points[i].x = x+scl*xs[i];
        fill_area.points[i].y = y+scl*ys[i];
    }
    gset_fill_int_style(istyle);
    gset_fill_colr_ind(icolor);
    gset_fill_style_ind(istndx);
/*
 * Fill area
 */
    gfill_area(&fill_area);
    free(fill_area.points);
    return(1);
}

