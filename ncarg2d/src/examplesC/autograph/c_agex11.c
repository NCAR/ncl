/*
 *	$Id: c_agex11.c,v 1.3 1994-08-01 22:15:09 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>

#define pow5(x)    ((x)*(x)*(x)*(x)*(x))

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{

/*
 * create a scattergram.
 */
    float xdra[500],ydra[500], x;
    int i;
    extern float fran();
    extern void bndary();
/*
 * initialize gks.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * fill the data arrays.
 */
    for( i = 0; i < 500; i++ ) {
        x = fran();
        xdra[i]=.5+pow5(2.*(x-.5));
        x = fran();
        ydra[i]=.5+pow5(2.*(x-.5));
    }
/*
 * draw a boundary around the edge of the plotter frame.
 */
    bndary();
/*
 * suppress the frame advance.
 */
    c_agseti("FRAME.",2);
/*
 * suppress the drawing of curves by the ez... routines.
 */
    c_agseti("SET.",-1);
/*
 * draw the background, using c_ezxy.
 */
    c_ezxy(xdra,ydra,500,"EXAMPLE 11 (SCATTERGRAM)$");
/*
 * put a plus sign at each of the x-y positions.
 */
    c_points(xdra,ydra,500,-2,0);
/*
 * advance the frame.
 */
    c_frame();
/*
 * close gks.
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


float fran()
{
/*
 * Pseudo-random-number generator.
 */
    static double x = 2.718281828459045;
    extern double fmod();
    x = fmod(9821.*x+.211327,1.);
    return((float)x);
}
