/*
 *	$Id: c_fgkgtx.c,v 1.1 1994-07-27 15:55:48 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	float plim1[2], plim2[2], plim3[2], plim4[2];
	extern void gtxexp();

	plim1[0] =  21.3; plim1[1] = 0.;
	plim2[0] =  -90.; plim2[1] = 0.;
	plim3[0] =  -55.; plim3[1] = 0.;
	plim4[0] =  -20.; plim4[1] = 0.;
/*
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Invoke demo driver
 */
	gtxexp("CE",-20.,-50.,0.,"PO","CO",plim1,plim2,plim3,plim4);
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();
}

void gtxexp(proj, plat, plon, rota, outln, jlim, plim1, plim2, plim3, plim4)
char *proj, *outln, *jlim;
float plat, plon, rota, *plim1, *plim2, *plim3, *plim4;
{
	extern void dfclrs();
	Gpoint pos;
    Gvec            vector;
	Gtext_font_prec txfp;
    float x, y;
	int i;
/*
 * PURPOSE                To provide a simple demonstration of the
 *                        GKS GTX character drawing techniques.
 *
 * Set up a color table
 */
	dfclrs();
/*
 * Draw Continental, political outlines 
 */
	c_mapstc ("OU - OUTLINE DATASET SELECTOR",outln);
/*
 * Set up projection
 */
	c_maproj (proj,plat,plon,rota);
/*
 * If it's a satellite projection, choose a satellite distance
 */
	  if (!strcmp(proj,"SV"))c_mapstr ("SA - SATELLITE DISTANCE",5.);
/*
 * Set limits of map
 */
	c_mapset (jlim,plim1,plim2,plim3,plim4);
/*
 * Turn off Grid lines
 */
	c_mapstr ("GR",0.);
/*
 * Turn off perimeter of map
 */
	c_mapsti ("PE", 0);
/*
 * Turn off map labels
 */
	c_mapsti ("LA", 0);
/*
 * Draw map
 */
	c_mapdrw();
/*
 * Label Brazil
 *
 * Set text color
 */
	gset_text_colr_ind(4);
/*
 *  Set text size
 */
	gset_char_ht(3.);
/*
 * Set the font
 */
	txfp.font = -16;
	txfp.prec = GPREC_STROKE;
	gset_text_font_prec(&txfp);
/*
 *  Transform the coordinates
 */
	c_maptra(-10.,-60.,&pos.x,&pos.y);
/*
 *  Draw the text
 */
	gtext(&pos,"Brazil");
/*
 * Label Argentina
 *
 * Set text color
 *
 *  gset_text_colr_ind(5)
 *
 *  Set text size
 */
	gset_char_ht(2.);
/*
 * Set the font
 */
	txfp.font = -14;
	gset_text_font_prec(&txfp);
/*
 * Transform the coordinates
 */
	c_maptra(-43.,-68.,&pos.x,&pos.y);
/*
 * Set the angle
 */
	vector.delta_x = -1.;
	vector.delta_y = .3;
	gset_char_up_vec(&vector);
/*
 * Draw the text
 */
	gtext(&pos,"Argentina");
/*
 * Label Uruguay
 *
 * Set text color
 */
	gset_text_colr_ind(6);
/*
 * Set text size
 */
	gset_char_ht(1.0);
/*
 * Set the font
 */
	txfp.font = -4;
	gset_text_font_prec(&txfp);
/*
 * Transform the coordinates
 */
	c_maptra(-32.5,-58.,&pos.x,&pos.y);
/*
 * Set the angle
 */
	vector.delta_x = .7;
	vector.delta_y = 1.;
	gset_char_up_vec (&vector);
/*
 * Draw the text
 */
	gtext(&pos,"Uruguay");
/*
 * Label Paraguay
 *
 * Set text color
 */
	gset_text_colr_ind(5);
/*
 * Set text size
 */
	  gset_char_ht(1.0);
/*
 * Set the font
 */
	txfp.font = -4;
	gset_text_font_prec(&txfp);
/*
 * Transform the coordinates
 */
	c_maptra(-21.,-61.,&pos.x,&pos.y);
/*
 * Set the angle
 */
	vector.delta_x = 1.;
	vector.delta_y = 1.;
	gset_char_up_vec (&vector);
/*
 * Draw the text
 */
	gtext(&pos,"Paraguay");
/*
 * Label Bolivia
 *
 * Set text color
 */
	gset_text_colr_ind(14);
/*
 * Set text size
 */
	gset_char_ht(1.6);
/*
 * Set the font
 */
	txfp.font = -6;
	gset_text_font_prec(&txfp);
/*
 * Transform the coordinates
 */
	c_maptra(-17.,-68.,&pos.x,&pos.y);
/*
 * Set the angle
 */
	vector.delta_x = 0.;
	vector.delta_y = 1.;
	gset_char_up_vec (&vector);
/*
 * Draw the text
 */
	gtext(&pos,"Bolivia");
/*
 * Label Chile
 *
 *  Set text color
 */
	gset_text_colr_ind(4);
/*
 * Set text size
 */
	gset_char_ht(1.5);
/*
 * Set the font
 */
	txfp.font = -7;
	gset_text_font_prec(&txfp);
/*
 * Transform the coordinates
 */
	c_maptra(-40.,-72.,&pos.x,&pos.y);
/*
 * Set the angle
 */
	vector.delta_x = -1.;
	vector.delta_y = .03;
	gset_char_up_vec (&vector);
/*
 * Expand the spacing between characters
 */
	gset_char_space(2.);
/*
 * Draw the text
 */
	gtext(&pos,"Chile");
/*
 * Label Peru
 *
 * Set text color
 */
	gset_text_colr_ind(14);
/*
 * Set text size
 */
	gset_char_ht(3.);
/*
 * Set the font
 */
	txfp.font = -15;
	gset_text_font_prec(&txfp);
/*
 * Transform the coordinates
 */
	c_maptra(-6.,-79.,&pos.x,&pos.y);
/*
 * Set the angle
 */
	vector.delta_x = -1.;
	vector.delta_y = 1.;
	gset_char_up_vec (&vector);
/*
 * Reset the spacing between characters
 */
	gset_char_space(0.);
/*
 *  Draw the text
 */
	gtext(&pos,"Peru");
/*
 * Label Equador
 *
 * Set text color
 */
	gset_text_colr_ind(15);
/*
 *  Set text size
 */
	gset_char_ht(1.5);
/*
 * Set the font
 */
	txfp.font = 1;
	gset_text_font_prec(&txfp);
/*
 * Transform the coordinates
 */
	c_maptra(0.,-86.,&pos.x,&pos.y);
/*
 * Set the angle
 */
	vector.delta_x = 0.;
	vector.delta_y = 1.;
	gset_char_up_vec (&vector);
/*
 * Draw the text
 */
	gtext(&pos,"Equador");
/*
 * Label Colombia
 *
 * Set text color
 */
	gset_text_colr_ind(1);
/*
 * Set text size
 */
	gset_char_ht(1.3);
/*
 *	   Set the font
 */
	txfp.font = -10;
	gset_text_font_prec(&txfp);
/*
 * Transform the coordinates
 */
	c_maptra(7.,-77.,&pos.x,&pos.y);
/*
 * Set the angle
 */
	vector.delta_x = .8;
	vector.delta_y = 1.;
	gset_char_up_vec (&vector);
/*
 * Draw the text
 */
	gtext(&pos,"Colombia");
/*
 * Label Venezuela
 *
 * Set text color
 */
	gset_text_colr_ind(3);
/*
 * Set text size
 */
	gset_char_ht(1.5);
/*
 *	   Set the font
 */
	txfp.font = -6;
	gset_text_font_prec(&txfp);
/*
 * Transform the coordinates
 */
	c_maptra(7.,-70.,&pos.x,&pos.y);
/*
 * Set the angle
 */
	vector.delta_x = 0.;
	vector.delta_y = 1.;
	gset_char_up_vec (&vector);
/*
 * Draw the text
 */
	gtext(&pos,"Venezuela");
/*
 * Label Guyana
 *
 * Set text color
 */
	gset_text_colr_ind(1);
/*
 * Set text size
 */
	gset_char_ht(1.0);
/*
 *	   Set the font
 */
	txfp.font = -4;
	gset_text_font_prec(&txfp);
/*
 * Transform the coordinates
 */
	c_maptra(7.,-59.5,&pos.x,&pos.y);
/*
 * Set the angle
 */
	vector.delta_x = 1.;
	vector.delta_y = .2;
	gset_char_up_vec (&vector);
/*
 * Draw the text
 */
	gtext(&pos,"Guyana");
/*
 * Label Fr. Guyana
 *
 * Set text color
 */
	gset_text_colr_ind(1);
/*
 * Set text size
 */
	gset_char_ht(1.2);
/*
 *	   Set the font
 */
	txfp.font = -4;
	gset_text_font_prec(&txfp);
/*
 * Transform the coordinates
 */
	c_maptra(2.,-53.5,&pos.x,&pos.y);
/*
 * Set the angle
 */
	vector.delta_x = -1.;
	vector.delta_y = .5;
	gset_char_up_vec (&vector);
/*
 * Draw the text
 */
	gtext(&pos,"Fr. Guyana");
/*
 * Label Suriname
 *
 * Set text color
 */
	gset_text_colr_ind(14);
/*
 * Set text size
 */
	gset_char_ht(1.2);
/*
 *	   Set the font
 */
	txfp.font = -4;
	gset_text_font_prec(&txfp);
/*
 * Transform the coordinates
 */
	c_maptra(2.5,-56.,&pos.x,&pos.y);
/*
 * Set the angle
 */
	vector.delta_x = -1.;
	vector.delta_y = .2;
	gset_char_up_vec (&vector);
/*
 * Draw the text
 */
	gtext(&pos,"Suriname");
/*
 * Label the plot
 *
 * Set text color
 */
	gset_text_colr_ind(1);
/*
 * Set text size
 */
	gset_char_ht(4.);
/*
 *	   Set the font
 */
	txfp.font = -14;
	gset_text_font_prec(&txfp);
/*
 * Transform the coordinates
 */
	c_maptra(15.,-80.,&pos.x,&pos.y);
/*
 * Set the angle
 */
	vector.delta_x = 0.;
	vector.delta_x = 1.;
	gset_char_up_vec (&vector);
/*
 * Draw the text
 */
	gtext (&pos,"South America");
/*
 * Draw a border around plot
 *
 * Reset the plot window
 */
	c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
/*
 * Set the line color to black
 */
	gset_line_colr_ind (1);
/*
 *  Create a background perimeter
 */
	c_frstpt( 0.0, 0.0);
	c_vector( 1.0, 0.0);
	c_vector( 1.0, 1.0);
	c_vector( 0.0, 1.0);
	c_vector( 0.0, 0.0);
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Done.
 */
	return;
}

void dfclrs()
{
	int i;
/*
 * Define a set of RGB color triples for colors 1 through 15.
 */
	Gcolr_rep rgb[16];
/*
 * Define the RGB color triples needed below.
 */
  	rgb[0].rgb.red = 1.00;  rgb[0].rgb.green = 1.00;  rgb[0].rgb.blue = 1.00;
	rgb[1].rgb.red = 0.00;  rgb[1].rgb.green = 0.00;  rgb[1].rgb.blue = 0.00;
	rgb[2].rgb.red = 0.70;  rgb[2].rgb.green = 0.70;  rgb[2].rgb.blue = 0.70;
	rgb[3].rgb.red = 0.75;  rgb[3].rgb.green = 0.50;  rgb[3].rgb.blue = 1.00;
	rgb[4].rgb.red = 0.50;  rgb[4].rgb.green = 0.00;  rgb[4].rgb.blue = 1.00;
	rgb[5].rgb.red = 0.00;  rgb[5].rgb.green = 0.00;  rgb[5].rgb.blue = 1.00;
	rgb[6].rgb.red = 0.00;  rgb[6].rgb.green = 0.50;  rgb[6].rgb.blue = 1.00;
	rgb[7].rgb.red = 0.00;  rgb[7].rgb.green = 1.00;  rgb[7].rgb.blue = 1.00;
	rgb[8].rgb.red = 0.00;  rgb[8].rgb.green = 1.00;  rgb[8].rgb.blue = 0.60;
	rgb[9].rgb.red = 0.00;  rgb[9].rgb.green = 1.00;  rgb[9].rgb.blue = 0.00;
	rgb[10].rgb.red = 0.70; rgb[10].rgb.green = 1.00; rgb[10].rgb.blue = 0.00;
	rgb[11].rgb.red = 1.00; rgb[11].rgb.green = 1.00; rgb[11].rgb.blue = 0.00;
	rgb[12].rgb.red = 1.00; rgb[12].rgb.green = 0.75; rgb[12].rgb.blue = 0.00;
	rgb[13].rgb.red = 1.00; rgb[13].rgb.green = 0.38; rgb[13].rgb.blue = 0.38;
	rgb[14].rgb.red = 1.00; rgb[14].rgb.green = 0.00; rgb[14].rgb.blue = 0.38;
	rgb[15].rgb.red = 1.00; rgb[15].rgb.green = 0.00; rgb[15].rgb.blue = 0.00;
/*
 * Define 16 different color indices, for indices 0 through 15.  The
 * color corresponding to index 0 is black and the color corresponding
 * to index 1 is white.
 */

	for( i = 0; i <= 15; i++ ) {
		gset_colr_rep (WKID,i,&rgb[i]);
	}
/*
 * Done.
 */
	return;
}
