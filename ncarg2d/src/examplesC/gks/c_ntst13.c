/*
 *	$Id: c_ntst13.c,v 1.2 1994-06-21 15:00:38 haley Exp $
 */
#include <stdio.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
/*
 *  Illustrate the various fonts.
 */
    char label[27];
    int i,j,ierr;
    float x,y;
    Gcolr_rep rgb1, rgb2;
    Gtext_align text_align;
    Gtext_font_prec text_font_prec;
    Gpoint text_pos;
/*
 *  Open GKS, open and activate the metafile workstation.
 */
    sprintf(label,"NCAR Graphics, Release 3.00");
    gopen_gks("stdout",0);
    gopen_ws( WKID, NULL, WSTYPE);
    gactivate_ws( WKID );
/*
 *  Define the necessary color indices.
 */
    rgb1.rgb.red = 0.; rgb1.rgb.green = 0.; rgb1.rgb.blue = .6;
    gset_colr_rep(WKID,0,&rgb1);
    ginq_colr_rep(WKID,0,0,&ierr,&rgb2);
    printf( "Testing ginq_colr_rep:  red = %g green = %g blue= %g\n", rgb1.rgb.red, rgb1.rgb.green, rgb1.rgb.blue );
    printf( "                        red = %g green = %g blue= %g\n", rgb2.rgb.red, rgb2.rgb.green, rgb2.rgb.blue );
    rgb1.rgb.red = 1.; rgb1.rgb.green = 1.; rgb1.rgb.blue = 0.;
    gset_colr_rep(WKID,1,&rgb1);
    rgb1.rgb.red = 0.; rgb1.rgb.blue = 1.;
    gset_colr_rep(WKID,2,&rgb1);
/*
 *  Set up character attributes.
 */
    gset_char_ht(.022);
    text_align.hor = GHOR_CTR;
    text_align.vert = GVERT_HALF;
    gset_text_align(&text_align);
    gset_text_colr_ind(1);
/*
 *  Loop through the fonts, the Hershey fonts have negative font
 *  indices.
 */
    x = .5;
    for(i=1;i<=20;i++){
        y = .1+.038*(20-i);
        j = i;
        if (i > 1) j=  (-i);
        text_font_prec.font = j;
        text_font_prec.prec = GPREC_STROKE;
        gset_text_font_prec(&text_font_prec);
        text_pos.x = x;
        text_pos.y = y;
        gtext(&text_pos,label);
    }
/*
 *  Label the plot (PLOTCHAR draws lines to plot characters,
 *  so the text color is controlled by the GKS polyline color).
 */
    gset_line_colr_ind(2);
    c_pcseti("CD",1);
    gset_linewidth(2.);
    c_plchhq(.5,.95,"Same string",.025,0.,0.);
    c_plchhq(.5,.90,"using various fonts",.025,0.,0.);
    c_frame();
/*
 *  Deactivate and close the workstation, close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}
