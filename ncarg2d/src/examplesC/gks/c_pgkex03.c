#include <math.h>
#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>


#define WSTYPE SED_WSTYPE
#define WKID   1

#define IWTYPE   1

main()
{
	Gcolr_rep rgb;
	int i;
	extern void drwtxt();
    float x[100], y[100];
/*
 *  Use inquiry functions to implement a subroutine (DRWTXT below)
 *  that puts out text using Normalized Device Coordinates (NDC) 
 *  for positioning.  Invoke Autograph to draw a linear/log plot
 *  and then call DRWTXT two times to label the Autograph plot.
 *
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks("stdout", 0 );
	gopen_ws(WKID, NULL, IWTYPE);
	gactivate_ws(WKID);
/*
 *  Define a small color table.
 */
	rgb.rgb.red = 1.; rgb.rgb.blue = 1.; rgb.rgb.green = 1.;
	gset_colr_rep(WKID,0,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,1,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,2,&rgb);
/*
 *  Generate a straight line with 100 points.
 */
	for( i = 0; i < 100; i++ ) {
        x[i] = i+1;
        y[i] = 10.*(i+1);
	}
/*
 *  Use SET to define normalization transformation 1 with linear 
 *  scaling in the X direction and log scaling in the Y direction.
 */
	c_set(.15,.85,.15,.85,1.,100.,10.,1000.,2);
/*
 *  Set line color to red.
 */
	gset_line_colr_ind(2);
/*
 *  Initialize the AUTOGRAPH entry EZXY so that the frame is not 
 *  advanced and the Y axis is logarithmic.  Turn off axis labels.
 */
	c_displa(2,0,2);
	c_anotat(" "," ",0,0,0,0);
/*
 *  Output the polyline (X,Y) using EZXY.
 */
	c_ezxy(x,y,100," ");
/*
 *  Put out a couple of labels (DRWTXT uses NDC space).
 */
	drwtxt(.50,.07,"The X axis is linear",-7,.025,0.);
	drwtxt(.07,.50,"The Y axis is log",-7,.025,90.);
/*
 *  Terminate the picture, deactivate and close the CGM workstation,
 *  and close GKS.
 */
	c_frame();
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void drwtxt(x,y,txt,ifnt,chgt,ang)
float x, y, ang;
Gdouble chgt;
char *txt;
int ifnt;
{
/*
 *  This subroutine draws the text string in TXT at position (X,Y) using
 *  font IFNT with character height CHGT (specified in NDC) and text 
 *  angle ANG degrees.  The position (X,Y) is in NDC. This subroutine 
 *  is isolated from any GKS attribute settings in the calling program 
 *  by using inquiry functions to save all settings on entry and restore 
 *  all settings on exit.  The text is aligned as (center, half) and is 
 *  drawn in the foreground color.  
 */
	int errind;
    Gclip clip_ind;
	float pi =3.1415927;
	Gtext_align txtal1, txtal2;
	Gtext_font_prec tfp1, tfp2;
	Gdouble chho;
	Gpoint pos;
    Gvec vector1, vector2;
	float xv1, xv2, yv1, yv2, xw1, xw2, yw1, yw2, rang;
	int ls;
/*
 *  Inquire and save the state of all attributes that will be used in
 *  this subroutine.  These will be restored on exit.
 *
 *   Clipping
 */
    ginq_clip(&errind,&clip_ind);
/*
 *   Character up vector.
 */
    ginq_char_up_vec(&errind,&vector1);
/*
 *   Text alignment.
 */
    ginq_text_align(&errind,&txtal1);
/*
 *   Text font.
 */
	ginq_text_font_prec(&errind,&tfp1);
/*
 *   Character height.
 */
	ginq_char_ht(&errind,&chho);
/*
 *  Get and save the existing normalization transformation information,
 *  including the log scaling parameter..
 */
	c_getset(&xv1,&xv2,&yv1,&yv2,&xw1,&xw2,&yw1,&yw2,&ls);
/*
 *  Use NDC space for drawing TXT.
 */
	gsel_norm_tran(0);
/*
 *  Define the text font.
 */
	tfp2.font = ifnt;
	tfp2.prec = GPREC_STROKE;
	gset_text_font_prec(&tfp2);
/*
 *  Set the character height.
 */
	gset_char_ht(chgt);
/*
 *  Set the text alignment to (center, half).
 */
	txtal2.hor = GHOR_CTR;
	txtal2.vert = GVERT_HALF;
	gset_text_align(&txtal2);
/*
 *  Select the foreground color.
 */
	gset_text_colr_ind(1);
/*
 *  Define the character up vector in accordance with ANG (recall that
 *  the up vector is perpendicular to the text path).
 */
	rang = (ang+90.)*(2.*pi/360.);
	vector2.delta_x = cos(rang);
	vector2.delta_y = sin(rang);
	gset_char_up_vec(&vector2);
/*
 *  Draw the text string in TXT.
 */
	pos.x = x;
	pos.y = y;
	gtext(&pos,txt);
/*
 *  Restore the original normalization transformation.
 */
	c_set(xv1,xv2,yv1,yv2,xw1,xw2,yw1,yw2,ls);
/*
 *  Restore all other attributes.
 */
	gset_clip_ind(clip_ind.clip_ind);
	gset_char_up_vec(&vector1);
	gset_text_align(&txtal1);
	gset_text_font_prec(&tfp1);
	gset_char_ht (chho);
	
	return;
}
