/*
 *	$Id: c_agex12.c,v 1.1 1994-05-13 14:24:21 haley Exp $
 */
#include <stdio.h>
#include <math.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>

#define pow2(x)    ((x)*(x))

main()
{

/*
 * Create a sort of histogram.
 */
    float xdra[249],ydra[249],work[204], franny;
    extern float fran();
    extern void bndary();
    int i, ndra, iwrk[204];
/*
 * initialize gks.
 */
    c_opngks();
/*
 * fill the data arrays.  first, we define the histogram
 * outline.  this will be used in the call to c_sfwrld which
 * fills in the area under the histogram.
 */
    xdra[0]=0.;
    ydra[0]=0.;

    for( i = 1; i < 100; i+=2 ) {
        xdra[i  ]=xdra[i-1];
        franny = (float)fran();
        ydra[i  ]=exp(-16.*pow2((float)((i+1)/2.)/50.-.51))+.1*franny;
        xdra[i+1]=xdra[i-1]+.02;
        ydra[i+1]=ydra[i];
    }

    xdra[101]=1.;
    ydra[101]=0.;
/*
 * define lines separating vertical boxes from each other.
 */
    ndra=101;

    for( i = 2; i < 99; i+= 2 ) {
        xdra[ndra+1]=1.e36;
        ydra[ndra+1]=1.e36;
        xdra[ndra+2]=xdra[i];
        ydra[ndra+2]=0.;
        xdra[ndra+3]=xdra[i];
        ydra[ndra+3]=ydra[i] < ydra[i+1] ? ydra[i] : ydra[i+1];
        ndra=ndra+3;
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
 * draw the graph, using c_ezxy.
 */
    c_ezxy(xdra,ydra,249,"EXAMPLE 12 (HISTOGRAM)$");
/*
 * use the package softfill to fill the area defined by the
 * data.
 */
    c_sfseti("AN",45);
    c_sfsetr("SP",.004);
    c_sfwrld(xdra,ydra,102,work,204,iwrk,204);
/*
 * advance the frame.
 */
    c_frame();
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
