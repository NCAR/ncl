/*
 *	$Id: c_sfex01.c,v 1.1 1994-05-13 14:28:49 haley Exp $
 */
#include <stdio.h>
#include <math.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

main()
{

/*
 * Declare required dimensioned arrays.
 */
    float xra[200],yra[200],dst[220];
    int ind[240],id1[8][8],id2[8][8],id3[8][8],id4[8][8];
    int i, j, k, l;
    float ycn, xcn;
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
 * Open GKS.
 */
    c_opngks();
/*
 * Double the size of the GKS dot.
 */
    gset_marker_size (2.);
/*
 * This code creates a single frame showing nine circles filled in
 * various ways.  The DO-loop variable I says which row of circles
 * we're working on (1 => top, 2 => middle, 3 => bottom).  The
 * DO-loop variable J says which column of circles we're working
 * on (1 => left, 2 => center, 3 => right).  The variable K gives
 * the number of the circle currently being drawn and is used in
 * a computed GO TO to determine which block of code is executed.
 */
    for( i = 1; i <= 3; i++ ) {
        ycn=(float)(4-i);
        for( j = 1; j <= 3; j++ ) {
            xcn=(float)(j);
            k=3*(i-1)+j;
            for( l = 0; l < 101; l++ ) {
                xra[l]=xcn+.48*sin(.062831853071796*(float)(l+1));
                yra[l]=ycn+.48*cos(.062831853071796*(float)(l+1));
            }
/*
 * Draw the circle.
 */
            c_set (0.,1.,0.,1.,0.,4.,0.,4.,1);
            c_curve (xra,yra,101);
/*
 * Jump to the proper piece of code to fill the circle.
 */
            switch( k ) {
              case 1:
/*
 * Fill the first circle with horizontal lines.
 */
                c_sfwrld (xra,yra,100,dst,102,ind,104);
                break;
/*
 * Fill the second circle in the same way, but add a diamond-shaped
 * hole.
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
                c_sfwrld (xra,yra,106,dst,110,ind,114);
                break;
/*
 * Fill the third circle with lines in two different directions to
 * create a cross-hatched effect and create a more complicated hole.
 */
              case 3:
                xra[100]=xra[ 39];
                yra[100]=yra[ 39];
                xra[101]=xra[ 79];
                yra[101]=yra[ 79];
                xra[102]=xra[ 19];
                yra[102]=yra[ 19];
                xra[103]=xra[ 59];
                yra[103]=yra[ 59];
                xra[104]=xra[ 99];
                yra[104]=yra[ 99];
                c_sfsetr ("SP - SPACING OF FILL LINES",.009);
                c_sfseti ("AN - ANGLE OF FILL LINES",45);
                c_sfwrld (xra,yra,105,dst,111,ind,117);
                c_sfseti ("AN - ANGLE OF FILL LINES",135);
                c_sfnorm (xra,yra,105,dst,111,ind,117);
                break;
/*
 * Fill the fourth circle with the default dot pattern, increasing the
 * inter-dot spacing considerably.
 */
              case 4:
                c_sfsetr ("SP - SPACING OF FILL LINES",.005);
                c_sfseti ("AN - ANGLE OF FILL LINES",0);
                c_sfseti ("DO - DOT-FILL FLAG",1);
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
                c_sfsetr ("SP - SPACING OF FILL LINES",.004);
                c_sfsetp (id1);
                c_sfwrld (xra,yra,100,dst,102,ind,104);
                break;
/*
 * Fill the seventh circle with a different dot pattern, tilted at an
 * angle.
 */
              case 7:
                c_sfseti ("AN - ANGLE OF FILL LINES",45);
                c_sfsetp (id2);
                c_sfwrld (xra,yra,100,dst,102,ind,104);
                break;
/*
 * Fill the eighth circle with a different dot pattern, using characters.
 */
              case 8:
                gset_char_ht (.004);
                c_sfsetr ("SP - SPACING OF FILL LINES",.006);
                c_sfseti ("AN - ANGLE OF FILL LINES",0);
                c_sfsetc ("CH - CHARACTER SPECIFIER","O");
                c_sfsetp (id3);
                c_sfwrld (xra,yra,100,dst,102,ind,104);
                break;
/*
 * Fill the last circle with K's, both large and small.
 */
              case 9:
                gset_char_ht (.008);
                c_sfsetr ("SP - SPACING OF FILL LINES",.012);
                c_sfsetc ("CH - CHARACTER SPECIFIER","K");
                c_sfsetp (id4);
                c_sfwrld (xra,yra,100,dst,102,ind,104);
                break;
            }
        }
    }
/*
 * Advance the frame.
 */
    c_frame();
/*
 * Close GKS.
 */
    c_clsgks();
}
