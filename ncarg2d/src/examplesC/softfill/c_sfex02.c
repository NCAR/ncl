/*
 *	$Id: c_sfex02.c,v 1.4 1997-04-21 14:38:37 haley Exp $
 */
#include <stdio.h>
#include <math.h>

/* 
 * Include function prototypes
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define IWTYPE 1
#define WKID   1

main()
{

/*
 * Declare required dimensioned arrays.
 */
    float xcs[101],ycs[101],xra[100],yra[100],dst[102];
    int ind[104];
    char lbl[3];
    int i, ity, ici, l;
    float xcn, ycn, x1, y1,x2,y2,rx,rx2;
    Gasfs ias;
    extern void dfclrs();
/*
 * Initialize the values in the aspect-source-flag array.
 */
    ias.linetype = GASF_INDIV;
    ias.linewidth = GASF_INDIV;
    ias.line_colr_ind = GASF_INDIV;
    ias.marker_type = GASF_INDIV;
    ias.marker_size = GASF_INDIV;
    ias.marker_colr_ind = GASF_INDIV;
    ias.text_font_prec = GASF_INDIV;
    ias.char_expan = GASF_INDIV;
    ias.char_space = GASF_INDIV;
    ias.text_colr_ind = GASF_INDIV;
    ias.fill_int_style = GASF_INDIV;
    ias.fill_style_ind = GASF_INDIV;
    ias.fill_colr_ind = GASF_INDIV;
/*
 * Open GKS.
 */
    gopen_gks("stdout",0);
    gopen_ws (WKID, NULL, IWTYPE);
    gactivate_ws(WKID);
/*
 * Turn off the clipping indicator.
 */
    gset_clip_ind(GIND_NO_CLIP);
/*
 * Set all the GKS aspect source flags to "individual".
 */
    gset_asfs (&ias);
/*
 * force solid fill.
 */
    gset_fill_int_style (GSTYLE_SOLID);
/*
 * Define color indices.
 */
    dfclrs();
/*
 * Define the window and viewport to make it easy to do seven rows
 * containing sixteen ovals apiece.
 */
    c_set (0.,1.,0.,1.,-1.,16.5,-.5,7.5,1);
/*
 * For each of the possible values of the internal parameter "TY", fill
 * a set of circles, one for each of sixteen values of ICI (0-15).
 */
    rx = 0.5;
    for( ity = -4; ity <= 2; ity++ ) {
        x1 = c_cufx(rx);
        rx2 = x1 - .006;
        x2 = c_cfux(rx2);
        ycn=(float)(3-ity);
        sprintf( lbl, "%2d", ity );
        c_plchhq (x2,ycn,lbl,.012,0.,1.);
        c_sfseti ("TYPE OF FILL",ity);

        for( ici= 0; ici <= 15; ici++ ) {
            xcn=(float)(ici+1);

            for( l = 0; l < 101; l++ ) {
                xcs[l]=xcn+.48*sin((double)(.062831853071796*(float)(l+1)));
                ycs[l]=ycn+.48*cos((double)(.062831853071796*(float)(l+1)));
                if (l < 100) {
                    xra[l]=xcs[l];
                    yra[l]=ycs[l];
                }
            }

            c_sfsgfa (xra,yra,100,dst,102,ind,104,ici);

            c_curve (xcs,ycs,101);

        }

    }
/*
 * Finish the labelling.
 */
    x1 = c_cufx(rx);
    rx2 = x1 - .060;
    x2 = c_cfux(rx2);
    c_plchhq (x2,4.,"TYPE OF FILL",.012,90.,0.);
    for( ici=0; ici <= 15; ici++ ) {
        y1 = c_cufy(rx);
        rx2 = y1 - .024;
        y2 = c_cfuy(rx2);
        xcn=(float)(ici+1);
        if (ici < 10) {
            sprintf( lbl, "%1d", ici );
            c_plchhq (xcn,y2,lbl,.012,0.,0.);
        }
        else {
            sprintf( lbl, "%2d", ici );
            c_plchhq (xcn,y2,lbl,.012,0.,0.);
        }
    }

    y1 = c_cufy(rx);
    rx2 = y1 - .060;
    y2 = c_cfuy(rx2);
    c_plchhq (8.5,y2,"COLOR INDEX",.012,0.,0.);
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
/*
 * Define a set of RGB color triples for colors 1 through 15.
 */
    float rgbv[3][15];
    Gcolr_rep color;
    int i;
/*
 * Define the RGB color triples needed below.
 */
    rgbv[0][0] = 1.00;    rgbv[1][0] = 1.00;    rgbv[2][0] = 1.00;
    rgbv[0][1] = 0.70;    rgbv[1][1] = 0.70;    rgbv[2][1] = 0.70;
    rgbv[0][2] = 0.75;    rgbv[1][2] = 0.50;    rgbv[2][2] = 1.00;
    rgbv[0][3] = 0.50;    rgbv[1][3] = 0.00;    rgbv[2][3] = 1.00;
    rgbv[0][4] = 0.00;    rgbv[1][4] = 0.00;    rgbv[2][4] = 1.00;
    rgbv[0][5] = 0.00;    rgbv[1][5] = 0.50;    rgbv[2][5] = 1.00;
    rgbv[0][6] = 0.00;    rgbv[1][6] = 1.00;    rgbv[2][6] = 1.00;
    rgbv[0][7] = 0.00;    rgbv[1][7] = 1.00;    rgbv[2][7] = 0.60;
    rgbv[0][8] = 0.00;    rgbv[1][8] = 1.00;    rgbv[2][8] = 0.00;
    rgbv[0][9] = 0.70;    rgbv[1][9] = 1.00;    rgbv[2][9] = 0.00;
    rgbv[0][10] = 1.00;    rgbv[1][10] = 1.00;    rgbv[2][10] = 0.00;
    rgbv[0][11] = 1.00;    rgbv[1][11] = 0.75;    rgbv[2][11] = 0.00;
    rgbv[0][12] = 1.00;    rgbv[1][12] = 0.38;    rgbv[2][12] = 0.38;
    rgbv[0][13] = 1.00;    rgbv[1][13] = 0.00;    rgbv[2][13] = 0.38;
    rgbv[0][14] = 1.00;    rgbv[1][14] = 0.00;    rgbv[2][14] = 0.00;
/*
 * Define 16 different color indices, for indices 0 through 15.  The
 * color corresponding to index 0 is black and the color corresponding
 * to index 1 is white.
 */
    color.rgb.red = color.rgb.green = color.rgb.blue = 0.;
    gset_colr_rep (WKID,0,&color);
    for( i = 0; i < 15; i++ ) {
        color.rgb.red = rgbv[0][i];
        color.rgb.green = rgbv[1][i];
        color.rgb.blue = rgbv[2][i];
        gset_colr_rep(WKID,i+1,&color);
    }
    return;
}
