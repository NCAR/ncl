/*
 *	$Id: c_fsrezsrf.c,v 1.1 1994-08-02 16:53:21 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define pow2(x)   ((x)*(x))

main()
{
	int ierr;
	extern void tsrfac();
/*
 * Open gks, open and activate a workstation.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Invoke demo driver
 */
	tsrfac(&ierr);
/*
 * Deactivate and close workstation, close gks.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

void tsrfac(ierror)
int *ierror;
{
	Gcolr_rep rgb;
    Gtext_align text_align;
	int i, j;
	float x, y;
/*
 * Purpose                to provide a simple demonstration of srface.
 *
 * usage                  call tsrfac (WKID,ierror)
 *
 * arguments
 *
 * on output              ierror
 *                      an integer variable
 *                      = 0, if the test was successful,
 *                      = 1, the test was not successful.
 *
 * i/o                    if the test is successful, the message
 *
 *           srface test executed--see plot to certify
 *
 *                    is printed on unit 6.  in addition, 1
 *                    frame is produced on the machine graphics
 *                    device.  in order to determine if the test
 *                    was successful, it is necessary to examine
 *                    the plot.
 *
 * precision              single
 *
 * language               fortran 77
 *
 * required routines      srface
 *
 * required gks level     0a
 *
 * algorithm              the function
 *
 *                      z(x,y) = .25*(x + y + 1./((x-.1)**2+y**2+.09)
 *                               -1./((x+.1)**2+y**2+.09)
 *
 *                    for x = -1. to +1. in increments of .1, and
 *                        y = -1.2 to +1.2 in increments of .1,
 *                    is computed.  then, entry surface
 *                    is called to a generate surface plot of z.
 *
 * history                surface was first written in april 1979 and
 *                    converted to fortran 77 and gks in march 1984.
 *
 * xx contains the x-direction coordinate values for z(x,y);  yy contains
 * the y-direction coordinate values for z(x,y);  z contains the function
 * values;  s contains values for the line of sight for entry srface;
 * work is a work array;  angh contains the angle in degrees in the x-y
 * plane to the line of sight;  and angv contains the angle in degrees
 * from the x-y plane to the line of sight.
 */
	float  xx[21],yy[25],z[25][21];
	float work[1096];

	float  s[6] = {-8.0, -6.0,  3.0,  0.0,  0.0,  0.0};

	float angh = 45., angv =15.;
/*
 * specify coordinates for plot titles.  The values cx and cy
 * Define the center of the title string in a 0. to 1. range.
 */
	Gpoint pos;
	pos.x = .5, pos.y = .9;
/*
 * Initialize the error parameter.
 */
	*ierror = 0;
/*
 * Set up a the background and foreground colors
 *
 * White background
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep (WKID,0,&rgb);
/*
 * Blue foreground
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
	gset_colr_rep (WKID,1,&rgb);
/*
 * Fill the xx and yy coordinate arrays as well as the z function array.
 */
	for( i = 1; i <= 21; i++ ) {
		x = .1*(float)(i-11);
		xx[i-1] = x;
		for( j = 1; j <= 25; j++ ) {
			y = .1*(float)(j-13);
			yy[j-1] = y;
			z[j-1][i-1] = (x+y+1./(pow2(x-.1)+y*y+.09)-1./(pow2(x+.1)+y*y+.09))*.25;
		}
	}
/*
 * Select the normalization transformation 0.
 */
	gsel_norm_tran(0);
/*
 * Add the plot title.
 *
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
	gtext(&pos,"DEMONSTRATION PLOT FOR EZSRFC ENTRY OF SRFACE");
/*
 * Draw the surface
 */
	c_ezsrfc (&z[0][0],21,25,angh,angv,&work[0]);
/*
 * This routine automatically generates frame advances.
 */
	return;
}

