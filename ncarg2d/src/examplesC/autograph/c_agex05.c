/*
 *	$Id: c_agex05.c,v 1.3 1994-08-08 22:27:31 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{

/*
 * Define the data arrays.
 */
    int i, j, ival;
    float xdra[6][401],ydra[6][401], wind[4];
    float pi, pid200, pittwo, pit2d3, pit4d3, radosc, radolc;
    float bsscll, bsscul, bslcll, bslcul, theta, rval;
    char stmp[20];
    extern void bndary();
/*
 *  Initialize
 */
    wind[0] = 0.;
    wind[1] = 1.;
    wind[2] = 0.;
    wind[3] = .5;
/*
 * Initialize GKS.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Compute required constants.
 */
    pi=3.14159265358979;
    pid200=pi/200.;
    pittwo=2.*pi;
    pit2d3=2.*pi/3.;
    pit4d3=4.*pi/3.;
    radosc=sqrt(3.)/3.;
    radolc=sqrt(3.)/2.;
    bsscll=atan(sqrt(12.)/6.);
    bsscul=atan(sqrt(143.)/7.);
    bslcll=atan(sqrt(143.)/17.);
    bslcul=atan(sqrt(2.0));
/*
 * fill the data arrays.
 */
    for( i = 0; i < 401; i++ ) {
        theta=pid200*(float)(i);
        xdra[0][i]=   -.5+radosc*cos((double)theta);
        ydra[0][i]=       radosc*sin((double)theta);
        if(fabs((double)theta) >= bsscll && fabs((double)theta) <= bsscul) xdra[0][i]=1.e36;
        if(fabs((double)(theta-pittwo)) >= bsscll && fabs((double)(theta-pittwo)) <= bsscul) xdra[0][i]=1.e36;
        xdra[1][i]=    .5+radosc*cos((double)theta);
        ydra[1][i]=       radosc*sin((double)theta);
        if(fabs((double)(theta-pit2d3)) >= bsscll && fabs((double)(theta-pit2d3)) <= bsscul) xdra[1][i]=1.e36;
        xdra[2][i]=       radosc*cos((double)theta);
        ydra[2][i]=radolc+radosc*sin((double)theta);
        if(fabs((double)(theta-pit4d3)) >= bsscll && fabs((double)(theta-pit4d3)) <= bsscul) xdra[2][i]=1.e36;
        xdra[3][i]=   -.5+radolc*cos((double)theta);
        ydra[3][i]=       radolc*sin((double)theta);
        if(fabs((double)theta) >= bslcll && fabs((double)theta) <= bslcul) xdra[3][i]=1.e36;
        if(fabs((double)(theta-pittwo)) >= bslcll &&  fabs((double)(theta-pittwo)) <= bslcul) xdra[3][i]=1.e36;
        xdra[4][i]=    .5+radolc*cos((double)theta);
        ydra[4][i]=       radolc*sin((double)theta);
        if(fabs((double)(theta-pit2d3)) >= bslcll && fabs((double)(theta-pit2d3)) <= bslcul) xdra[4][i]=1.e36;
        xdra[5][i]=       radolc*cos((double)theta);
        ydra[5][i]=radolc+radolc*sin((double)theta);
        if(fabs((double)(theta-pit4d3)) >= bslcll && fabs((double)(theta-pit4d3)) <= bslcul) xdra[5][i]=1.e36;
    }
/*
 * specify subscripting of xdra and ydra.
 */
    c_agseti("ROW.",2);
/*
 * set up grid shape to make 1 unit in x = 1 unit in y.
 */
    c_agsetf("GRID/SHAPE.",2.);
/*
 * turn off background, then turn labels back on.
 */
    c_agsetf("BACKGROUND.",4.);
    c_agseti("LABEL/CONTROL.",2);
/*
 * turn off left label.
 */
    c_agsetc("LABEL/NAME.","L");
    c_agseti("LABEL/SUPPRESSION FLAG.",1);
/*
 * change text of bottom label.
 */
    c_agsetc("LABEL/NAME.","B");
    c_agseti("LINE/NUMBER.",-100);
/*
 * Set LINE/TEXT
 */
    c_agsetc("LINE/TEXT.","PURITY, BODY, AND FLAVOR$");
/*
 * draw a boundary around the edge of the plotter frame.
 */
/*
 * draw the graph, using c_ezmxy.
 */
    c_ezmxy(&xdra[0][0],&ydra[0][0],401,6,401,"EXAMPLE 5 (EZMXY)$");
    bndary();
/*
 * Test c_aggetc
 */
    c_agsetc("LINE/TEXT.","THIS IS A TEST$");
    c_aggetc("LINE/TEXT.",stmp,19);
	if( strcmp( stmp, "THIS IS A TEST" ) ) {
		printf( "c_aggetc:  stmp should be 'THIS IS A TEST', stmp is really '%s'\n", stmp );
	}
/*
 * Test c_aggetf
 */
    c_agsetf("BACKGROUND.",3.);
    c_aggetf("BACKGROUND.",&rval);
	if( rval != 3. ) {
		printf( "c_aggetf: rval should be 3., rval is really %g\n", rval );
	}
/*
 * Test c_aggetr
 */
    c_agsetr("BACKGROUND.",2.);
    c_aggetr("BACKGROUND.",&rval);
	if( rval != 2. ) {
		printf( "c_agsetr, c_aggetr: rval should be 2., rval is really %g\n", rval );
	}
/*
 * Test c_aggeti
 */
    c_agseti("LINE/NUMBER.",5);
    c_aggeti("LINE/NUMBER.",&ival);
	if( ival != 5 ) {
		printf( "c_aggeti: ival should be 5, ival is really %d\n", ival );
	}
/*
 * Test c_aggetp
 */
    wind[0] = 0.;
    wind[1] = 1.;
    wind[2] = 0.;
    wind[3] = .5;
	c_agsetp("GRAPH WINDOW.",wind,4);
    c_aggetp("GRAPH WINDOW.",wind,4);
	if( wind[0] != 0. || wind[1] != 1. || wind[2] != 0. || wind[3] != .5 ) {
		printf( "c_aggetp: wind should be 0.,1.,0.,.5, wind is really %g %g %g %g\n", wind[0], wind[1], wind[2], wind[3] );
	}
/*
 * Close GKS
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

