/*
 *	$Id: c_agex01.c,v 1.1 1994-05-13 14:24:06 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>

#define MAX  1001

main()
{
    float x, ydra[MAX], franny;
    extern float fran();
    extern void bndary();
    int i;
/*
 *  Initialize GKS.
 */
      c_opngks();
/*
 *  Fill the data array.
 */
    for( i = 0; i < MAX; i++ ) {
        x=(float)(i+1)/20.;
        franny = (float)fran();
        ydra[i]=10.*(x-1.)*(x-11.)*(x-21.)*(x-31.)*(x-41.)*
                    (x-51.)+2.e7*(franny-.5);
    }
/* 
 *  Draw a boundary around the edge of the plotter frame.
 */
    bndary();
/*
 *  Draw the graph, using EZY.
 */
    c_ezy(ydra,MAX,"EXAMPLE 1 (EZY)$");
/*
 * Close GKS.
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

float fran()
{
/*
 * Pseudo-random-number generator.
 */
    static double x = 2.718281828459045;
    extern double fmod();
    x = fmod(9821.*x+.211327, 1.);
    return((float)x);
}
