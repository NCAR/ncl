/*
 *	$Id: c_tsrfac.c,v 1.1 1994-05-13 14:29:15 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define pow2(x)    ((x)*(x))

main()
{
    int idum, ierr;
/*
 * OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
 */
    gopen_gks ("stdout",0);
    gopen_ws (1, NULL, 1);
    gactivate_ws (1);
/*
 * INVOKE DEMO DRIVER
 */
    tsrfac(ierr);
/*
 *     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
    gdeactivate_ws(1);
    gclose_ws(1);
    gclose_gks();
}
tsrfac (ierror)
int ierror;
{
    int i, j;
    float cx, cy, x, y, angh, angv;
    float xx[21],yy[25],z[25][21],s[6], work[1096];
    int iwk[25][21][2];
    Gtext_align text_align;
    Gpoint text_pos;

    s[0] = -8.0;
    s[1]= -6.0;
    s[2] = 3.0;
    s[3] = s[4] = s[5] = 0.0;
    angh = 45.;
    angv = 15.;
/*
 * Specify coordinates for plot titles.  The values CX and CY
 * define the center of the title string in a 0. to 1. range.
 */
    text_pos.x = .5;
    text_pos.y = .9;
/*
 * Initialize the error parameter.
 */
    ierror = 0;
/*
 * Fill the XX and YY coordinate arrays as well as the Z function array.
 */
    for( i = 0; i < 21; i++ ) {
        x = .1*(float)(i-10);
        xx[i] = x;
        for( j = 0; j < 25; j++ ) {
            y = .1*(float)(j-12);
            yy[j] = y;
            z[j][i] = (x+y+1./(pow2(x-.1)+y*y+.09)-1./(pow2(x+.1)+y*y+.09))*.25;
        }
    }
/*
 * Select the normalization transformation 0.
 */
    gsel_norm_tran(0);
/*
 *     Frame 1 -- The EZSRFC entry.
 */

/*
 * Add the plot title using GKS calls.
 */
/*
 * Set the text alignment to center the string in horizontal and vertical
 */
    text_align.hor = GHOR_CTR;
    text_align.vert = GVERT_HALF;
    gset_text_align(&text_align);
/*
 * Set the character height.
 */
    gset_char_ht(.016);
/*
 * Write the text.
 */
    gtext(&text_pos,"DEMONSTRATION PLOT FOR EZSRFC ENTRY OF SRFACE");

    c_ezsrfc (&z[0][0],21,25,angh,angv,work);
/*
 *     Frame 2 -- The SRFACE entry.
 */

/*
 * Set the text alignment to center the string in horizontal and vertical
 */
    gset_text_align(&text_align);
/*
 * Set the character height.
 */
    gset_char_ht(.016);
/*
 * Write the text.
 */
    gtext(&text_pos,"DEMONSTRATION PLOT FOR SRFACE ENTRY OF SRFACE");

    c_srface (xx,yy,&z[0][0],&iwk[0][0][0],21,21,25,s,0.);
/*
 * This routine automatically generates frame advances.
 */
    printf( "SRFACE TEST EXECUTED--SEE PLOT TO CERTIFY\n" );
    return(1);
}
