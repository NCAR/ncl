/*
 *	$Id: c_elblba.c,v 1.3 1995-06-14 13:59:42 haley Exp $
 */
#include <stdio.h>
#include <math.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

char *llb1[20] = {
    "  0 to 5:H2Q"," 5 to 10:H1Q","10 to 15:H1Q","15 to 20:H1Q","20 to 25:H1Q",
    "25 to 30:H1Q","30 to 35:H1Q","35 to 40:H1Q","40 to 45:H1Q","45 to 50:H1Q",
    "50 to 55:H1Q","55 to 60:H1Q","60 to 65:H1Q","65 to 70:H1Q","70 to 75:H1Q",
    "75 to 80:H1Q","80 to 85:H1Q","85 to 90:H1Q","90 to 95:H1Q","   95 to 100"
};

char *llb2[17] = 
{
    "-2000 feet"," Sea Level"," 2000 feet"," 4000 feet"," 6000 feet",
    " 8000 feet","10000 feet","12000 feet","14000 feet","16000 feet",
    "18000 feet","20000 feet","22000 feet","24000 feet","26000 feet",
    "28000 feet","30000 feet"
};

char *llb3[4] = {"M","N","O","P"};
char *llb4[4] = {"I","J","K","L"};
char *llb5[4] = {"E","F","G","H"};
char *llb6[4] = {"A","B","C","D"};

#define IWTYPE 1
#define WKID   1

main()
{
    int i;
    Gasfs iasf;
    int lnd1[20],lnd2[16],lnd3[4],lnd4[4],lnd5[4],lnd6[4];
    extern void dfclrs();

    iasf.linetype = GASF_INDIV;
    iasf.linewidth = GASF_INDIV;
    iasf.line_colr_ind = GASF_INDIV;
    iasf.marker_type = GASF_INDIV;
    iasf.marker_size = GASF_INDIV;
    iasf.marker_colr_ind = GASF_INDIV;
    iasf.text_font_prec = GASF_INDIV;
    iasf.char_expan = GASF_INDIV;
    iasf.char_space = GASF_INDIV;
    iasf.text_colr_ind = GASF_INDIV;
    iasf.fill_int_style = GASF_INDIV;
    iasf.fill_style_ind = GASF_INDIV;
    iasf.fill_colr_ind = GASF_INDIV;

    for( i = 0; i < 20; i++ ) lnd1[i] = i+1;
    for( i = 0; i < 16; i++ ) lnd2[i] = i;
    for( i = 0; i < 4; i++ ) {
        lnd3[i] = i+12;
        lnd4[i] = i+8;
        lnd5[i] = i+4;
        lnd6[i] = i;
    }
/*
 * Open GKS.
 */
    gopen_gks("stdout",0);
    gopen_ws (WKID, NULL, IWTYPE);
    gactivate_ws(WKID);
/*
 * Set all the GKS aspect source flags to "individual".
 */
    gset_asfs(&iasf);
/*
 * Force solid fill.
 */
    gset_fill_int_style(GSTYLE_SOLID);
/*
 * Turn off the clipping indicator.
 */
    gset_clip_ind(GIND_NO_CLIP);
/*
 * Define color indices.
 */
    dfclrs();
/*
 * Set some parameter values.
 */
    c_pcsetr("CS - CONSTANT SPACING",1.25);
    c_lbsetr("WBL - WIDTH OF BOX LINES",4.);
    c_lbsetr("WFL - WIDTH OF FILL LINES",2.);
    c_lbsetr("WLB - WIDTH OF LABEL LINES",2.);
/*
 * Put the first label bar vertically along the left edge of the plotter
 * frame.  Use patterns.
 */
    c_sfseti("ANGLE OF FILL LINES",15);
    c_sfseti ("TYPE OF FILL",-4);
    c_lblbar(1,.05,.30,.05,.95,20,.3333,1.,lnd1,0,llb1,20,2);
/*
 * Put the second label bar vertically along the right edge.  Use solid
 * color fill.
 */
    c_sfseti("TYPE OF FILL",0);
    c_lblbar(1,.70,.95,.05,.95,16,.3333,1.,lnd2,0,llb2,17,1);
    c_lblbar(0,.35,.65,.05,.20,4,.5,.5,lnd3,1,llb3,4,1);
    c_lblbar(0,.35,.65,.20,.35,4,.5,.5,lnd4,1,llb4,4,1);
    c_lblbar(0,.35,.65,.35,.50,4,.5,.5,lnd5,1,llb5,4,1);
    c_lblbar(0,.35,.65,.50,.65,4,.5,.5,lnd6,1,llb6,4,1);
/*
 * Put a title on the plot.  We must first call SET to define the ranges
 * of the X and Y coordinates to be used.  The constant spacing feature
 * is turned off so that the title will look normal.
 */
    c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
    c_pcsetr("CS - CONSTANT SPACING",0.);
    c_plchhq(.5,.90,"THREE",.025,0.,0.);
    c_plchhq(.5,.85,"LABELBAR",.025,0.,0.);
    c_plchhq(.5,.80,"EXAMPLES",.025,0.,0.);
/*
 * Advance the frame.
 */    
    c_frame();
/*
 * Close GKS.
 */
    gdeactivate_ws (WKID);
    gclose_ws (WKID);
    gclose_gks();
}

void dfclrs()
{
    int i;
    Gcolr_rep rgb[16];
/*
 * Define a set of RGB color triples for colors 1 through 15.
 */
    rgb[0].rgb.red = rgb[0].rgb.green = rgb[0].rgb.blue = 0.0;
    rgb[1].rgb.red = rgb[1].rgb.green = rgb[1].rgb.blue = 1.00;
    rgb[2].rgb.red = 0.70; rgb[2].rgb.green = 0.70; rgb[2].rgb.blue = 0.70;
    rgb[3].rgb.red = 0.75; rgb[3].rgb.green = 0.50; rgb[3].rgb.blue = 1.00;
    rgb[4].rgb.red = 0.50; rgb[4].rgb.green = 0.00; rgb[4].rgb.blue = 1.00;
    rgb[5].rgb.red = 0.00; rgb[5].rgb.green = 0.00; rgb[5].rgb.blue = 1.00;
    rgb[6].rgb.red = 0.00; rgb[6].rgb.green = 0.50; rgb[6].rgb.blue = 1.00;
    rgb[7].rgb.red = 0.00; rgb[7].rgb.green = 1.00; rgb[7].rgb.blue = 1.00;
    rgb[8].rgb.red = 0.00; rgb[8].rgb.green = 1.00; rgb[8].rgb.blue = 0.60;
    rgb[9].rgb.red = 0.00; rgb[9].rgb.green = 1.00; rgb[9].rgb.blue = 0.00;
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
        gset_colr_rep(WKID,i,&rgb[i]);
    }
}
