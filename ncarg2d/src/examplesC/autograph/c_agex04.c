/*
 *	$Id: c_agex04.c,v 1.1 1994-05-13 14:24:10 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>

#define pow2(x)    ((x)*(x))

main()
{

/*
 * Define the data arrays.
 */
    int i, j;
    float xdra[201],ydra[10][201];
    extern void bndary();
/*
 * Initialize GKS.
 */
    c_opngks();
    
    for( i = 0; i < 201; i++ ) {
        xdra[i] = -1.+.02*(float)(i);
        if( i > 100 ) xdra[i]=2.-xdra[i];
        for( j = 0; j < 10; j++ ) {
            ydra[j][i]=(float)(j+1)*sqrt(1.000000000001-pow2(xdra[i]))/10.;
            if(i > 100) ydra[j][i]= -ydra[j][i];
        }
    }
    bndary();
/*
 * Draw the graph, using EZMXY.
 */
    c_ezmxy(xdra,&ydra[0][0],201,10,201,"EXAMPLE 4 (EZMXY)$");
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

