/*
 *	$Id: c_agex02.c,v 1.1 1994-05-13 14:24:07 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>

main()
{

/*
 * Define the data arrays.
 */
    int i;
    float xdra[4001],ydra[4001];
    float theta,rho;
    extern void bndary();
/*
 * Initialize GKS.
 */
    c_opngks();
/*
 * Fill the data arrays.
 */
    for( i = 0; i < 4001; i++ ) {
        theta=.0015707963267949*(float)(i);
        rho=sin(2.*theta)+.05*sin(64.*theta);
        xdra[i]=rho*cos(theta);
        ydra[i]=rho*sin(theta);
    }
/*
 * Draw a boundary around the edge of the plotter frame.
 */
    bndary();
/*
 * Draw the graph, using EZXY.
 */
    c_ezxy(xdra,ydra,4001,"EXAMPLE 2 (EZXY)$");
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

