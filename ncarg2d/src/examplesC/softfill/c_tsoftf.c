/*
 *	$Id: c_tsoftf.c,v 1.1 1994-05-13 14:28:50 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

main()
{
/*
 * OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
 */
    int idum, ierr;
    gopen_gks ("stdout",0);
    gopen_ws (1, NULL, 1);
    gactivate_ws (1);
/*
 * INVOKE DEMO DRIVER
 */
    tsoftf(&ierr);
/*
 *     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
    gdeactivate_ws(1);
    gclose_ws(1);
    gclose_gks();
}

tsoftf (ierr)
int *ierr;
{
/*
 * LATEST REVISION       July, 1989
 */

/*
 * Declare required dimensioned arrays.
 */
    float xra[200],yra[200],dst[220],xsv[101],ysv[101], rval;
    int ind[240],id1[8][8],id2[8][8],id3[8][8],id4[8][8];
    int i, j, k, l, ival, isetp;
    char stmp[6];
    float ycn, xcn;
    Gasfs iasf;
/*
 * Initialize the values in the aspect-source-flag array.
 */
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
/*
 * Define four different dot patterns.
 */
    id1[0][0] = 1;
    id1[1][0] = 1;
    id1[2][0] = 0;
    id1[3][0] = 0;
    id1[4][0] = 0;
    id1[5][0] = 0;
    id1[6][0] = 1;
    id1[7][0] = 1;
    id1[0][1] = 1;
    id1[1][1] = 1;
    id1[2][1] = 0;
    id1[3][1] = 1;
    id1[4][1] = 1;
    id1[5][1] = 0;
    id1[6][1] = 1;
    id1[7][1] = 1;
    id1[0][2] = 0;
    id1[1][2] = 0;
    id1[2][2] = 0;
    id1[3][2] = 1;
    id1[4][2] = 1;
    id1[5][2] = 0;
    id1[6][2] = 0;
    id1[7][2] = 0;
    id1[0][3] = 0;
    id1[1][3] = 1;
    id1[2][3] = 1;
    id1[3][3] = 1;
    id1[4][3] = 1;
    id1[5][3] = 1;
    id1[6][3] = 1;
    id1[7][3] = 0;
    id1[0][4] = 0;
    id1[1][4] = 1;
    id1[2][4] = 1;
    id1[3][4] = 1;
    id1[4][4] = 1;
    id1[5][4] = 1;
    id1[6][4] = 1;
    id1[7][4] = 0;
    id1[0][5] = 0;
    id1[1][5] = 0;
    id1[2][5] = 0;
    id1[3][5] = 1;
    id1[4][5] = 1;
    id1[5][5] = 0;
    id1[6][5] = 0;
    id1[7][5] = 0;
    id1[0][6] = 1;
    id1[1][6] = 1;
    id1[2][6] = 0;
    id1[3][6] = 1;
    id1[4][6] = 1;
    id1[5][6] = 0;
    id1[6][6] = 1;
    id1[7][6] = 1;
    id1[0][7] = 1;
    id1[1][7] = 1;
    id1[2][7] = 0;
    id1[3][7] = 0;
    id1[4][7] = 0;
    id1[5][7] = 0;
    id1[6][7] = 1;
    id1[7][7] = 1;

    id2[0][0] = 0;
    id2[1][0] = 0;
    id2[2][0] = 0;
    id2[3][0] = 0;
    id2[4][0] = 0;
    id2[5][0] = 0;
    id2[6][0] = 0;
    id2[7][0] = 0;
    id2[0][1] = 0;
    id2[1][1] = 1;
    id2[2][1] = 1;
    id2[3][1] = 1;
    id2[4][1] = 1;
    id2[5][1] = 1;
    id2[6][1] = 1;
    id2[7][1] = 0;
    id2[0][2] = 0;
    id2[1][2] = 1;
    id2[2][2] = 1;
    id2[3][2] = 1;
    id2[4][2] = 1;
    id2[5][2] = 1;
    id2[6][2] = 1;
    id2[7][2] = 0;
    id2[0][3] = 0;
    id2[1][3] = 1;
    id2[2][3] = 1;
    id2[3][3] = 0;
    id2[4][3] = 0;
    id2[5][3] = 1;
    id2[6][3] = 1;
    id2[7][3] = 0;
    id2[0][4] = 0;
    id2[1][4] = 1;
    id2[2][4] = 1;
    id2[3][4] = 0;
    id2[4][4] = 0;
    id2[5][4] = 1;
    id2[6][4] = 1;
    id2[7][4] = 0;
    id2[0][5] = 0;
    id2[1][5] = 1;
    id2[2][5] = 1;
    id2[3][5] = 1;
    id2[4][5] = 1;
    id2[5][5] = 1;
    id2[6][5] = 1;
    id2[7][5] = 0;
    id2[0][6] = 0;
    id2[1][6] = 1;
    id2[2][6] = 1;
    id2[3][6] = 1;
    id2[4][6] = 1;
    id2[5][6] = 1;
    id2[6][6] = 1;
    id2[7][6] = 0;
    id2[0][7] = 0;
    id2[1][7] = 0;
    id2[2][7] = 0;
    id2[3][7] = 0;
    id2[4][7] = 0;
    id2[5][7] = 0;
    id2[6][7] = 0;
    id2[7][7] = 0;

    id3[0][0] = 0;
    id3[1][0] = 0;
    id3[2][0] = 0;
    id3[3][0] = 0;
    id3[4][0] = 0;
    id3[5][0] = 0;
    id3[6][0] = 0;
    id3[7][0] = 0;
    id3[0][1] = 0;
    id3[1][1] = 0;
    id3[2][1] = 0;
    id3[3][1] = 0;
    id3[4][1] = 1;
    id3[5][1] = 0;
    id3[6][1] = 0;
    id3[7][1] = 0;
    id3[0][2] = 0;
    id3[1][2] = 0;
    id3[2][2] = 0;
    id3[3][2] = 1;
    id3[4][2] = 1;
    id3[5][2] = 1;
    id3[6][2] = 0;
    id3[7][2] = 0;
    id3[0][3] = 0;
    id3[1][3] = 1;
    id3[2][3] = 0;
    id3[3][3] = 0;
    id3[4][3] = 1;
    id3[5][3] = 0;
    id3[6][3] = 0;
    id3[7][3] = 1;
    id3[0][4] = 0;
    id3[1][4] = 0;
    id3[2][4] = 1;
    id3[3][4] = 1;
    id3[4][4] = 1;
    id3[5][4] = 1;
    id3[6][4] = 1;
    id3[7][4] = 0;
    id3[0][5] = 0;
    id3[1][5] = 0;
    id3[2][5] = 0;
    id3[3][5] = 0;
    id3[4][5] = 1;
    id3[5][5] = 0;
    id3[6][5] = 0;
    id3[7][5] = 0;
    id3[0][6] = 0;
    id3[1][6] = 0;
    id3[2][6] = 0;
    id3[3][6] = 1;
    id3[4][6] = 0;
    id3[5][6] = 1;
    id3[6][6] = 0;
    id3[7][6] = 0;
    id3[0][7] = 0;
    id3[1][7] = 1;
    id3[2][7] = 1;
    id3[3][7] = 0;
    id3[4][7] = 0;
    id3[5][7] = 0;
    id3[6][7] = 1;
    id3[7][7] = 1;

    id4[0][0] = 0;
    id4[1][0] = 0;
    id4[2][0] = 0;
    id4[3][0] = 0;
    id4[4][0] = 0;
    id4[5][0] = 0;
    id4[6][0] = 0;
    id4[7][0] = 0;
    id4[0][1] = 0;
    id4[1][1] = 1;
    id4[2][1] = 1;
    id4[3][1] = 0;
    id4[4][1] = 0;
    id4[5][1] = 1;
    id4[6][1] = 1;
    id4[7][1] = 1;
    id4[0][2] = 0;
    id4[1][2] = 1;
    id4[2][2] = 1;
    id4[3][2] = 0;
    id4[4][2] = 0;
    id4[5][2] = 1;
    id4[6][2] = 1;
    id4[7][2] = 0;
    id4[0][3] = 0;
    id4[1][3] = 1;
    id4[2][3] = 1;
    id4[3][3] = 0;
    id4[4][3] = 1;
    id4[5][3] = 1;
    id4[6][3] = 0;
    id4[7][3] = 0;
    id4[0][4] = 0;
    id4[1][4] = 1;
    id4[2][4] = 1;
    id4[3][4] = 1;
    id4[4][5] = 1;
    id4[5][5] = 0;
    id4[6][5] = 0;
    id4[7][5] = 0;
    id4[0][5] = 0;
    id4[1][5] = 1;
    id4[2][5] = 1;
    id4[3][5] = 0;
    id4[4][5] = 1;
    id4[5][5] = 1;
    id4[6][5] = 0;
    id4[7][5] = 0;
    id4[0][6] = 0;
    id4[1][6] = 1;
    id4[2][6] = 1;
    id4[3][6] = 0;
    id4[4][6] = 0;
    id4[5][6] = 1;
    id4[6][6] = 1;
    id4[7][6] = 0;
    id4[0][7] = 0;
    id4[1][7] = 1;
    id4[2][7] = 1;
    id4[3][7] = 0;
    id4[4][7] = 0;
    id4[5][7] = 1;
    id4[6][7] = 1;
    id4[7][7] = 1;
/*
 * Initialize the error parameter.
 */
    *ierr=0;
/*
 * Double the size of the GKS dot.
 */
    gset_marker_size (2.);
/*
 * Set all the GKS aspect source flags to "individual".
 */
    gset_asfs (&iasf);
/*
 * Force solid fill.
 */
    gset_fill_int_style (GSTYLE_SOLID);
/*
 * Turn off the clipping indicator.
 */
    gset_clip_ind (GIND_NO_CLIP);
/*
 * Define a bunch of color indices.
 */
    sfclrs();
/*
 * Do a set call allowing us to use X and Y coordinates in the range
 * from 0 to 4.
 */
    c_set (0.,1.,0.,1.,0.,4.,0.,4.,1);
/*
 * Put a label at the top of the frame.
 */
    c_plchlq(2.,3.75,"DEMONSTRATION PLOT FOR SOFTFILL",16.,0.,0.);
/*
 * Return the basic internal parameters to their default values.
 */
    c_sfseti ("AN",0);
    c_sfseti ("CH",0);
    c_sfseti ("DO",0);
    c_sfsetr ("SP",.00125);
    c_sfseti ("TY",0);
/*
 * The following code creates a single frame showing nine circles filled
 * in various ways.  The DO-loop variable I says which row of circles
 * we"re working on (1 => top, 2 => middle, 3 => bottom).  The DO-loop
 * variable J says which column of circles we"re working C on (1 =>
 * left, 2 => center, 3 => right).  The variable K gives the number of
 * the circle currently being drawn and is used in a computed GO TO to
 * determine which block of code is executed.
 */
    for( i = 1; i <= 3; i++ ) {
        ycn=(float)(4-i);
        for( j = 1; j <= 3; j++ ) {
            xcn=(float)j;
            k=3*(i-1)+j;
/*
 * Generate the coordinates defining the circle.  Two sets of arrays are
 * used, one set for use in calling the fill routines, which transform
 * the contents of the arrays, and one set for use in drawing the circle.
 */
            for( l = 0; l < 101; l++ ) {
                xra[l]=xcn+.48*sin(.062831853071796*(float)(l+1));
                xsv[l]=xra[l];
                yra[l]=ycn+.48*cos(.062831853071796*(float)(l+1));
                ysv[l]=yra[l];
            }
/*
 * Jump to the proper piece of code to fill the circle.
 */
            switch(k) {
/*
 * Fill the first circle in color number 5.
 */
              case 1:
                c_sfseti ("TYPE OF FILL",0);
                c_sfsgfa (xra,yra,100,dst,102,ind,104,5);
                break;
/*
 * Add a diamond-shaped hole to the circle and fill it in color number
 * 9, using lines in two directions to effect the fill.
 */
              case 2:
                xra[100]=2.00;
                yra[100]=3.24;
                xra[101]=1.76;
                yra[101]=3.00;
                xra[102]=2.00;
                yra[102]=2.76;
                xra[103]=2.24;
                yra[103]=3.00;
                xra[104]=xra[100];
                yra[104]=yra[100];
                xra[105]=xra[99];
                yra[105]=yra[99];
                c_sfseti ("TYPE OF FILL",2);
                c_sfsgfa (xra,yra,106,dst,110,ind,114,9);
                break;
/*
 * Create a more complicated hole in the third circle and fill it with
 * pattern number 11 of the 20 or so that can be created using "TY"=-4.
 */
              case 3:
                xra[100]=xra[39];
                yra[100]=yra[39];
                xra[101]=xra[79];
                yra[101]=yra[79];
                xra[102]=xra[19];
                yra[102]=yra[19];
                xra[103]=xra[59];
                yra[103]=yra[59];
                xra[104]=xra[99];
                yra[104]=yra[99];
                c_sfseti ("TYPE OF FILL",-4);
                c_sfsgfa (xra,yra,105,dst,111,ind,117,11);
                c_sfseti ("ANGLE OF FILL LINES",15);
                break;
/*
 * Fill the fourth circle with the default dot pattern, increasing the
 * inter-dot spacing considerably.
 */
              case 4:
                c_sfsetr ("SPACING OF FILL LINES",.005);
                c_sfseti ("ANGLE OF FILL LINES",0);
                c_sfseti ("DOT-FILL FLAG",1);
                c_sfwrld (xra,yra,100,dst,102,ind,104);
                break;
/*
 * Fill the fifth circle with a combination of lines and dots.
 */
              case 5:
                c_sfsetr ("SP - SPACING OF FILL LINES",.012);
                c_sfseti ("DO - DOT-FILL FLAG",0);
                c_sfwrld (xra,yra,100,dst,102,ind,104);
                c_sfsetr ("SP - SPACING OF FILL LINES",.006);
                c_sfseti ("DO - DOT-FILL FLAG",1);
                c_sfnorm (xra,yra,100,dst,102,ind,104);
                break;
/*
 * Fill the sixth circle with a specified dot pattern.
 */
              case 6:
                c_sfsetr ("SPACING OF FILL LINES",.004);;
                c_sfsetp (id1);
                c_sfwrld (xra,yra,100,dst,102,ind,104);
                break;
/*
 * Fill the seventh circle with a different dot pattern, tilted at an
 * angle.
 */
              case 7:
                c_sfseti ("ANGLE OF FILL LINES",45);
                c_sfsetp (id2);
                c_sfwrld (xra,yra,100,dst,102,ind,104);
                break;
/*
 * Fill the eighth circle with a different dot pattern, using characters.
 */
              case 8:
                gset_char_ht  (.004);
                c_sfsetr ("SPACING OF FILL LINES",.006);
                c_sfseti ("ANGLE OF FILL LINES",0);
                c_sfsetc ("CHARACTER SPECIFIER","O");
                c_sfsetp (id3);
                c_sfwrld (xra,yra,100,dst,102,ind,104);
                break;
/*
 * Fill the last circle with K"s, both large and small.
 */
              case 9:
                gset_char_ht  (.008);
                c_sfsetr ("SPACING OF FILL LINES",.012);
                c_sfsetc ("CHARACTER SPECIFIER","K");
                c_sfsetp (id4);
                c_sfwrld (xra,yra,100,dst,102,ind,104);
                break;
/*
 * Draw the circle.
 */
            }
            c_curve (xsv,ysv,101);

        }

    }
/*
 * Advance the frame.
 */
    c_frame();
/*
 * Log execution message and return to caller.
 */
    printf(" SOFTFILL TEST EXECUTED--SEE PLOT TO CERTIFY\n");
/*
 * Test c_sfgetc
 */
    c_sfsetc("CHARACTER SPECIFIER","P");
    c_sfgetc("CHARACTER SPECIFIER",stmp,5);
    printf( "\nc_sfgetc:  stmp should be 'P', stmp is really '%s'\n", stmp );
/*
 * Test c_sfgeti
 */
    c_sfseti("ANGLE OF FILL LINES",20);
    c_sfgeti("ANGLE OF FILL LINES",&ival);
    printf( "\nc_sfgeti:  ival should be 20, ival is really %d\n", ival );
/*
 * Test c_sfgetr
 */
    c_sfsetr ("SPACING OF FILL LINES",.015);
    c_sfgetr ("SPACING OF FILL LINES",&rval );
    printf( "\nc_sfgetr:  rval should be 0.015 rval is really %g\n", rval );
/*
 * Test c_sfgetp
 */
	c_sfsetp (id4);
	c_sfgetp (id3);
    isetp = 1;
	for( i = 0; i < 8; i++ ) {
		for( j = 0; j < 8; j++ ) {
            if( id3[i][j] != id4[i][j] ) {
				printf( "\nc_sfgetp test UNSUCCESSFUL\n" );
				isetp = 0;
				break;
			}
        }
    }
    if( isetp ) printf( "\nc_sfgetp test SUCCESSFUL\n" );
}

sfclrs()
{
    Gcolr_rep rgbv[16];
    int i;
/*
 * Define the RGB color triples needed below.
 */
    rgbv[0].rgb.red = 0.0;
    rgbv[0].rgb.green = 0.0;
    rgbv[0].rgb.blue = 0.0;
    rgbv[1].rgb.red = 1.00;
    rgbv[1].rgb.green = 1.00;
    rgbv[1].rgb.blue = 1.00;
    rgbv[2].rgb.red = 0.70;
    rgbv[2].rgb.green = 0.70;
    rgbv[2].rgb.blue = 0.70;
    rgbv[3].rgb.red = 0.75;
    rgbv[3].rgb.green = 0.50;
    rgbv[3].rgb.blue = 1.00;
    rgbv[4].rgb.red = 0.50;
    rgbv[4].rgb.green = 0.00;
    rgbv[4].rgb.blue = 1.00;
    rgbv[5].rgb.red = 0.00;
    rgbv[5].rgb.green = 0.00;
    rgbv[5].rgb.blue = 1.00;
    rgbv[6].rgb.red = 0.00;
    rgbv[6].rgb.green = 0.50;
    rgbv[6].rgb.blue = 1.00;
    rgbv[7].rgb.red = 0.00;
    rgbv[7].rgb.green = 1.00;
    rgbv[7].rgb.blue = 1.00;
    rgbv[8].rgb.red = 0.00;
    rgbv[8].rgb.green = 1.00;
    rgbv[8].rgb.blue = 0.60;
    rgbv[9].rgb.red = 0.00;
    rgbv[9].rgb.green = 1.00;
    rgbv[9].rgb.blue = 0.00;
    rgbv[10].rgb.red = 0.70;
    rgbv[10].rgb.green = 1.00;
    rgbv[10].rgb.blue = 0.00;
    rgbv[11].rgb.red = 1.00;
    rgbv[11].rgb.green = 1.00;
    rgbv[11].rgb.blue = 0.00;
    rgbv[12].rgb.red = 1.00;
    rgbv[12].rgb.green = 0.75;
    rgbv[12].rgb.blue = 0.00;
    rgbv[13].rgb.red = 1.00;
    rgbv[13].rgb.green = 0.38;
    rgbv[13].rgb.blue = 0.38;
    rgbv[14].rgb.red = 1.00;
    rgbv[14].rgb.green = 0.00;
    rgbv[14].rgb.blue = 0.38;
    rgbv[15].rgb.red = 1.00;
    rgbv[15].rgb.green = 0.00;
    rgbv[15].rgb.blue = 0.00;
/*
 * Define 16 different color indices, for indices 0 through 15.  The
 * color corresponding to index 0 is black and the color corresponding
 * to index 1 is white.
 */
    for( i = 0; i <= 15; i++ ) {
        gset_colr_rep(1,i,&rgbv[i]);
    }
    return(1);
}
