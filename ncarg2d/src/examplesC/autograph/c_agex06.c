/*
 *	$Id: c_agex06.c,v 1.2 1994-06-21 14:58:39 haley Exp $
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
    float xdra[501],ydra[501], gwnd[4][4], gwnd2[4], theta;
    extern void bndary();
/*
 * Define variables used in setting up informational labels.
 */
    char glab[36], back[4][24],lnlg[4][13];
    int i, j, igrf, illx, illy;
    
    gwnd[0][0] = 0.0; gwnd[1][0] = 0.5; gwnd[2][0] = 0.5; gwnd[3][0] = 1.0;
    gwnd[0][1] = 0.5; gwnd[1][1] = 1.0; gwnd[2][1] = 0.5; gwnd[3][1] = 1.0;
    gwnd[0][2] = 0.0; gwnd[1][2] = 0.5; gwnd[2][2] = 0.0; gwnd[3][2] = 0.5;
    gwnd[0][3] = 0.5; gwnd[1][3] = 1.0; gwnd[2][3] = 0.0; gwnd[3][3] = 0.5;

    strcpy( back[0], "(PERIMETER BACKGROUND)$" );
    strcpy( back[1], "(GRID BACKGROUND)$     " );
    strcpy( back[2], "(HALF-AXIS BACKGROUND)$" );
    strcpy( back[3], "(NO BACKGROUND)$       " );

    strcpy( lnlg[0], "LINEAR$     " );
    strcpy( lnlg[1], "LOGARITHMIC$" );
/*
 * initialize gks.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * fill the data arrays.
 */
    for( i = 0; i < 501; i++ ) {
        theta=.031415926535898*(float)(i);
        xdra[i]=500.+.9*(float)(i)*cos(theta);
        ydra[i]=500.+.9*(float)(i)*sin(theta);
    }
/*
 * suppress the frame advance.
 */
    c_agseti("FRAME.",2);
/*
 * do four graphs on the same frame, using different
 * backgrounds.
 */
    for( igrf = 0; igrf < 4; igrf++ ) {
/*
 * position the graph window.
 */
        for( j = 0; j < 4; j++ ) gwnd2[j] = gwnd[j][igrf];
        c_agsetp("GRAPH WINDOW.",gwnd2,4);
/*
 * declare the background type.
 */
        c_agseti("BACKGROUND TYPE.",igrf+1);
/*
 * setting the background type may have turned informational
 * labels off.  in that case, turn them back on.
 */
        if(igrf == 3) c_agseti("LABEL/CONTROL.",2);
/*
 * set up parameters determining linear/log nature of axes.
 */
        illx=(igrf)/2;
        illy=(igrf) % 2;
/*
 * declare the linear/log nature of the graph.
 */
        c_agseti("X/LOGARITHMIC.",illx);
        c_agseti("Y/LOGARITHMIC.",illy);
/*
 * change the x- and y-axis labels to reflect the linear/log
 * nature of the graph.
 */
        c_agsetc("LABEL/NAME.","B");
        c_agseti("LINE/NUMBER.",-100);
        c_agsetc("LINE/TEXT.",lnlg[illx]);

        c_agsetc("LABEL/NAME.","L");
        c_agseti("LINE/NUMBER.",100);
        c_agsetc("LINE/TEXT.",lnlg[illy]);
/*
 * set up the label for the top of the graph.
 */

        sprintf( glab,"EXAMPLE 6-%d %23s", igrf+1, back[igrf] );
/*
 * draw the graph, using c_ezxy.
 */
        c_ezxy(xdra,ydra,501,glab);

    }
/*
 * draw a boundary around the edge of the plotter frame.
 */
    bndary();
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

