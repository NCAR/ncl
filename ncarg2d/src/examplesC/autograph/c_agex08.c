/*
 *	$Id: c_agex08.c,v 1.1 1994-05-13 14:24:15 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>

main()
{

/*
 * Define the data arrays.
 */
    int i, j;
    float xdra[101],ydra[101][4], base, flti;
    extern void bndary();
/*
 * Initialize GKS.
 */
    c_opngks();
/*
 * Fill the data arrays.
 */
    for( i = 0; i < 101; i++ ) {
        xdra[i]= -3.14159265358979+.062831853071796*(float)(i);
    }
    for( i = 0; i < 4; i++ ) {
        flti=(float)(i+1);
        base=2.*flti-1.;
        for( j = 0; j < 101; j++ ) {
            ydra[j][i]=base+.75*sin(-3.14159265358979+
                       .062831853071796*flti*(float)(j));
        }
    }
/*
 * change the line-end character to a period.
 */
    c_agsetc("LINE/END.",".");
/*
 * specify labels for x and y axes.
 */
    c_anotat("SINE FUNCTIONS OF T.","T.",0,0,0,NULL);
/*
 * use a half-axis background.
 */
    c_agseti("BACKGROUND.",3);
/*
 * move x axis to the zero point on the y axis.
 */
    c_agsetf("BOTTOM/INTERSECTION/USER.",0.);
/*
 * specify base value for spacing of major ticks on x axis.
 */
    c_agsetf("BOTTOM/MAJOR/BASE.",1.);
/*
 * run major ticks on x axis to edge of curve window.
 */
    c_agsetf("BOTTOM/MAJOR/INWARD.",1.);
    c_agsetf("BOTTOM/MAJOR/OUTWARD.",1.);
/*
 * position x axis minor ticks.
 */
    c_agseti("BOTTOM/MINOR/SPACING.",9);
/*
 * run the y axis backward.
 */
    c_agseti("Y/ORDER.",1);
/*
 * run plots full-scale in y.
 */
    c_agseti("Y/NICE.",0);
/*
 * have autograph scale x and y data the same.
 */
    c_agsetf("GRID/SHAPE.",.01);
/*
 * use the alphabetic set of dashed-line patterns.
 */
    c_agseti("DASH/SELECTOR.",-1);
/*
 * tell autograph how the data arrays are dimensioned.
 */
    c_agseti("ROW.",-1);
/*
 * reverse the roles of the x and y arrays.
 */
    c_agseti("INVERT.",1);
/*
 * draw a boundary around the edge of the plotter frame.
 */
    bndary();
/*
 * draw the curves.
 */
    c_ezmxy(xdra,&ydra[0][0],4,4,101,"EXAMPLE 8.");
/*
 * close gks.
 */
    c_clsgks();

}


void bndary()
{
/*
 * Routine to draw the plotter-frame edge.
 */
    c_plotit(    0,    0,0);
    c_plotit(32767,    0,1);
    c_plotit(32767,32767,1);
    c_plotit(    0,32767,1);
    c_plotit(    0,    0,1);
}

