/*
 *	$Id: c_agex07.c,v 1.3 1995-06-14 13:58:58 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <string.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>

#define pow2(x)  ((x) * (x))

#define IWTYPE 1
#define WKID   1

int main()
{

/*
 * Define the data arrays and the dash-pattern array.
 */
    float xdra[101],ydra[9][101];
    char dshp[9][29], stmp[17];
    int i, j, fj;
#ifdef NeedFuncProto
    extern char *agdshn(int);
#else
    extern char *agdshn();
#endif
    extern void bndary();
/*
 * initialize gks.
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, IWTYPE);
	gactivate_ws(WKID);
/*
 * fill the data arrays and the dash pattern array.
 */
    for( i = 0; i < 101; i++ ) {
        xdra[i]= -90.+1.8*(float)(i);
    }
    for( j = 0; j < 9; j++ ) {
        sprintf( dshp[j], "$$$$$$$$$$$$$$$$$$$$$'j'='%d'", j+1 );
        fj=j+1;
        for( i = 0; i < 101; i++ ) {
            ydra[j][i]=3.*fj-(fj/2700.)*pow2(xdra[i]);
        }
    }
/*
 * turn on windowing.
 */
    c_agseti("WINDOWING.",1);
/*
 * move the edges of the curve window (grid).
 */
    c_agsetf("GRID/LEFT."  ,.10);
    c_agsetf("GRID/RIGHT." ,.90);
    c_agsetf("GRID/BOTTOM.",.10);
    c_agsetf("GRID/TOP."   ,.85);
/*
 * set the x and y minimum and maximum.
 */
    c_agsetf("X/MINIMUM.",-90.);
    c_agsetf("X/MAXIMUM.", 90.);
    c_agsetf("Y/MINIMUM.",  0.);
    c_agsetf("Y/MAXIMUM.", 18.);
/*
 * set left axis parameters.
 */
    c_agseti("LEFT/MAJOR/TYPE.",1);
    c_agsetf("LEFT/MAJOR/BASE.",3.);
    c_agseti("LEFT/MINOR/SPACING.",2);
/*
 * set right axis parameters.
 */
    c_agseti("RIGHT/FUNCTION.",1);
    c_agsetf("RIGHT/NUMERIC/TYPE.",1.e36);
/*
 * set bottom axis parameters.
 */
    c_agseti("BOTTOM/MAJOR/TYPE.",1);
    c_agsetf("BOTTOM/MAJOR/BASE.",15.);
    c_agseti("BOTTOM/MINOR/SPACING.",2);
/*
 * set top axis parameters.
 */
    c_agseti("TOP/FUNCTION.",2);
    c_agsetf("TOP/NUMERIC/TYPE.",1.e36);
/*
 * set up the dash patterns to be used.
 */
    c_agseti("DASH/SELECTOR.",9);
    c_agseti("DASH/LENGTH.",28);
    for( i = 0; i < 9; i++ ) {
        strcpy( stmp, c_agdshn(i+1));
        c_agsetc(stmp,dshp[i]);
    }
/*
 * set up the left label.
 */
    c_agsetc("LABEL/NAME.","L");
    c_agseti("LINE/NUMBER.",100);
    c_agsetc("LINE/TEXT.","HEIGHT (KILOMETERS)$");
/*
 * set up the right label.
 */
    c_agsetc("LABEL/NAME.","R");
    c_agseti("LINE/NUMBER.",-100);
    c_agsetc("LINE/TEXT.","PRESSURE (TONS/SQUARE FURLONG)$");
/*
 * set up the bottom labels.
 */
    c_agsetc("LABEL/NAME.","B");
    c_agseti("LINE/NUMBER.",-100);
    c_agsetc("LINE/TEXT.","LATITUDE (DEGREES)$");

    c_agsetc("LABEL/NAME.","SP");
    c_agsetf("LABEL/BASEPOINT/X.",.000001);
    c_agsetf("LABEL/BASEPOINT/Y.",0.);
    c_agsetf("LABEL/OFFSET/Y.",-.015);
    c_agseti("LINE/NUMBER.",-100);
    c_agsetc("LINE/TEXT.","SP$");

    c_agsetc("LABEL/NAME.","NP");
    c_agsetf("LABEL/BASEPOINT/X.",.999999);
    c_agsetf("LABEL/BASEPOINT/Y.",0.);
    c_agsetf("LABEL/OFFSET/Y.",-.015);
    c_agseti("LINE/NUMBER.",-100);
    c_agsetc("LINE/TEXT.","NP$");
/*
 * set up the top label.
 */
    c_agsetc("LABEL/NAME.","T");
    c_agseti("LINE/NUMBER.",80);
    c_agsetc("LINE/TEXT.","DISTANCE FROM EQUATOR (MILES)$");
    c_agseti("LINE/NUMBER.",90);
    c_agsetc("LINE/TEXT."," $");
    c_agseti("LINE/NUMBER.",100);
    c_agsetc("LINE/TEXT.","LINES OF CONSTANT INCRUDESCENCE$");
    c_agseti("LINE/NUMBER.",110);
    c_agsetc("LINE/TEXT.","EXAMPLE 7 (EZMXY)$");
/*
 * set up centered (box 6) label.
 */
    c_agsetc("LABEL/NAME.","EQUATOR");
    c_agseti("LABEL/ANGLE.",90);
    c_agseti("LINE/NUMBER.",0);
    c_agsetc("LINE/TEXT.","EQUATOR$");
/*
 * draw a boundary around the edge of the plotter frame.
 */
    bndary();
/*
 * draw the graph, using c_ezmxy.
 */
    c_ezmxy (xdra,&ydra[0][0],101,9,101,"");
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

NGCALLF(agutol,AGUTOL)(iaxs,funs,idma,vinp,votp)
int *iaxs, *idma;
float *funs, *vinp, *votp;
{
/*
 * mapping for the right axis.
 */
    if ( *funs == 1. ) {
        if (*idma > 0) *votp = log10(20. - *vinp);
        if (*idma < 0) *votp = 20.- pow(10.,*vinp);
/*
 * mapping for the top axis.
 */
    }
    else {
        if ( *funs == 2.) {
            if (*idma > 0) *votp = 70.136 * *vinp;
            if (*idma < 0) *votp = *vinp/70.136;
/*
 * default (identity) mapping.
 */
        }
        else {
            *votp = *vinp;
        }
    }
    return(1);
}
