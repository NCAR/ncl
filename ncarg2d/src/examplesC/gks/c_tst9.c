/*
 *	$Id: c_tst9.c,v 1.3 1994-06-27 20:26:44 haley Exp $
 */
#include <stdio.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID 1 

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
    sprintf(label,"NCAR Graphics, Release 4.0");
    gopen_gks("stdout",0);
    gopen_ws( WKID, NULL, WSTYPE);
    gactivate_ws( WKID );
/*
 *  Define the necessary color indices.
 */
    rgb1.rgb.red = 0.; rgb1.rgb.green = 0.; rgb1.rgb.blue = .6;
    gset_colr_rep(WKID,0,&rgb1);
    ginq_colr_rep(WKID,0,0,&ierr,&rgb2);
    if((rgb2.rgb.red != rgb1.rgb.red) || (rgb2.rgb.green != rgb1.rgb.green) || (rgb2.rgb.blue != rgb1.rgb.blue)){
        printf("\n ginq_colr_rep test failed\n");
    }else {
        printf("\n ginq_colr_rep test passed\n");
    } 
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
        y = .02+.043*(20-i);
        c_pcseti( "FN", i );
        if (i == 18) c_pcsetr( "AS", .25 );
        if (i == 19) c_pcsetr( "AS", .5 );
        if (i == 20) c_pcsetr( "AS", .25 );
        c_plchhq(x,y,label,.02,0.,0.);
    }
/*
 *  Label the plot (PLOTCHAR draws lines to plot characters,
 *  so the text color is controlled by the GKS polyline color).
 */
    c_pcseti("FN",25);
    c_pcseti("CC",2);
    c_pcsetr("AS",0.);
    c_plchhq(.5,.98,"Same string",.032,0.,0.);
    c_plchhq(.5,.92,"using various fonts",.032,0.,0.);
    c_frame();
/*
 *  Deactivate and close the workstation, close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}
