/*
 *	$Id: c_agex03.c,v 1.2 1994-06-21 14:58:36 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>

#define pow2(x)    ((x)*(x))

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{

/*
 * Define the data array.
 */
    int i;
    float ydra[2][100];
    extern void bndary();
/*
 * Initialize GKS.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Fill the data array.
 */
    for( i = 0; i < 100; i++ ) {
        ydra[0][i]=cos(3.14159265358979*(float)(i+1)/25.)*pow2((float)(i+1));
        ydra[1][i]=cos(3.14159265358979*(float)(i+1)/25.)*pow(10.,(.04*(float)(i+1)));
    }
/*
 * Draw a boundary around the edge of the plotter frame.
 */
    bndary();
/*
 * Draw the graph, using EZMY.
 */
    c_ezmy(&ydra[0][0],100,2,100,"EXAMPLE 3 (EZMY)$");
/*
 * Close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
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

