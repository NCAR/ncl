/*
 *	$Id: c_arex01.c.sed,v 1.1 1994-05-13 14:25:15 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define LAMA 10000
#define NCRA 1000
#define NGPS 10

main()
{
/*
 * This example code forces ARPRAM to produce, for a simple area map, a
 * set of plots showing all the edge segments in the area map at each of
 * the steps that are done to preprocess the area map.  The user may wish
 * to modify this example to look at what happens to other simple area
 * maps.  For a complicated area map, though, the technique will probably
 * produce an unreadable set of plots.
 *
 *
 * The last plot produced by this example shows "contour bands" over a
 * simple "map".  The colors used over "water" range from green at one
 * end of the contour range to red at the other end.  Over "land", we
 * use a color defined by an RGB triple in which the red and green
 * components are what they would have been over "water", but we add
 * a maximal blue component to wash out the color into a pastel shade.
 * A rectangular area (in which, one might imagine, data were missing)
 * is masked out so as to be left uncolored.
 *
 * Declare the area map array.
 */
    int iama[LAMA],iaai[NGPS],iagi[NGPS];
/*
 * Declare the work arrays to be used by ARSCAM.
 */
    float xcra[NCRA],ycra[NCRA];
/*
 * Declare arrays to hold a perimeter, to be used for all three groups
 * of edges.
 */
    float perix[5],periy[5];
/*
 * Declare arrays for edge group 1, which may be thought of as defining
 * a simple geographic map, showing a single continent, in the middle of
 * which is is a lake.
 */
    float g1e1x[9],g1e1y[9], g1e2x[5],g1e2y[5];
/*
 * Declare arrays for edge group 3, which may be thought of as defining
 * a contour map.  A "contour line" that lies on top of one of the others
 * and has contradictory area identifiers is thrown in to show what AREAS
 * does in such cases.
 */
    float g3e1x[2], g3e1y[2], g3e2x[2], g3e2y[2], g3e3x[2], g3e3y[2];
    float g3e4x[2], g3e4y[2], g3e5x[2], g3e5y[2], g3e6x[2], g3e6y[2];
    float g3e7x[2], g3e7y[2], g3e8x[2], g3e8y[2], g3e9x[2], g3e9y[2];
/*
 * Declare arrays for edge group 5, which may be thought of as defining
 * a "blocked" region, inside of which we don't want to show anything.
 * This edge group is also used to show examples of coincident edge
 * segments and "dangling" edge segments.
 */
    float g5e1x[7],g5e1y[7];
    int i, ival;
    float s;
    Gcolr_rep colr;
/*
 * Declare the routine that will color the areas defined by the area map.
 */
    extern int colram(
#ifdef NeedFuncProto
    float *xcra,
    float *ycra,
    int *ncra,
    int *iaai,
    int *iagi,
    int *ngps
#endif
    );
/*
 * Define the perimeter for all three groups of edges.
 */
    perix[0] = .90;
    perix[1] = .90;
    perix[2] = .10;
    perix[3] = .10;
    perix[4] = .90;
    periy[0] = .10;
    periy[1] = .90;
    periy[2] = .90;
    periy[3] = .10;
    periy[4] = .10;
/*
 * Define the group 1 edges.
 */
    g1e1x[0] = .75;    g1e1y[0] = .50;
    g1e1x[1] = .68;    g1e1y[1] = .68;
    g1e1x[2] = .50;    g1e1y[2] = .75;
    g1e1x[3] = .32;    g1e1y[3] = .68;
    g1e1x[4] = .25;    g1e1y[4] = .50;
    g1e1x[5] = .32;    g1e1y[5] = .32;
    g1e1x[6] = .50;    g1e1y[6] = .25;
    g1e1x[7] = .68;    g1e1y[7] = .32;
    g1e1x[8] = .75;    g1e1y[8] = .50;
    
    g1e2x[0] = .60;    g1e2y[0] = .40;
    g1e2x[1] = .60;    g1e2y[1] = .60;
    g1e2x[2] = .40;    g1e2y[2] = .60;
    g1e2x[3] = .40;    g1e2y[3] = .40;
    g1e2x[4] = .60;    g1e2y[4] = .40;
/*
 * Define the group 3 edges.
 */
    g3e1x[0] = .10;    g3e1y[0] = .80;
    g3e1x[1] = .20;    g3e1y[1] = .90;
    g3e2x[0] = .10;    g3e2y[0] = .60;
    g3e2x[1] = .40;    g3e2y[1] = .90;
    g3e3x[0] = .10;    g3e3y[0] = .40;
    g3e3x[1] = .60;    g3e3y[1] = .90;
    g3e4x[0] = .10;    g3e4y[0] = .20;
    g3e4x[1] = .80;    g3e4y[1] = .90;
    g3e5x[0] = .20;    g3e5y[0] = .10;
    g3e5x[1] = .90;    g3e5y[1] = .80;
    g3e6x[0] = .40;    g3e6y[0] = .10;
    g3e6x[1] = .90;    g3e6y[1] = .60;
    g3e7x[0] = .60;    g3e7y[0] = .10;
    g3e7x[1] = .90;    g3e7y[1] = .40;
    g3e8x[0] = .80;    g3e8y[0] = .10;
    g3e8x[1] = .90;    g3e8y[1] = .20;
    g3e9x[0] = .40;    g3e9y[0] = .70;
    g3e9x[1] = .20;    g3e9y[1] = .50;
/*
 * Define the group 5 edges.
 */
    g5e1x[0] = .50;    g5e1y[0] = .50;
    g5e1x[1] = .80;    g5e1y[1] = .50;
    g5e1x[2] = .80;    g5e1y[2] = .80;
    g5e1x[3] = .50;    g5e1y[3] = .80;
    g5e1x[4] = .50;    g5e1y[4] = .50;
    g5e1x[5] = .20;    g5e1y[5] = .20;
    g5e1x[6] = .35;    g5e1y[6] = .35;
/*
 * Open GKS.
 */
    c_opngks();
/*
 * Change the GKS "fill area interior style" to be "solid".
 */
    gset_fill_int_style (GSTYLE_SOLID);
/*
 * Define the colors to be used.  Color indices 0 and 1 are the default
 * background and foreground colors (black and white, respectively).
 * Color indices 11 through 19 are to be used over "ocean" and color
 * indices 21 through 29 are to be used over "land".
 */
    colr.rgb.red = colr.rgb.green = colr.rgb.blue = 0.;
    gset_colr_rep (1,0,&colr);
    colr.rgb.red = colr.rgb.green = colr.rgb.blue = 1.;
    gset_colr_rep (1,1,&colr);
    
    for( i=1; i <= 9; i++ ) {
        s=(float)(i)/9.;
        colr.rgb.red = s;
        colr.rgb.green = 1.-s;
        colr.rgb.blue = 0.;
        gset_colr_rep (1,10+i,&colr);
        colr.rgb.blue = 1.;
        gset_colr_rep (1,20+i,&colr);
    }
/*
 * Define the mapping from the user system to the plotter frame.
 */
    c_set (.05,.95,.05,.95,0.,1.,0.,1.,1);
/*
 * Turn on the AREAS debugging flag.  This will cause the routine
 * ARDBPA to be called by ARPRAM.  For the purposes of this example,
 * we supply our own version of ARDBPA, which has been hacked to show
 * the contents of the area map at each breakpoint position for groups
 * 1, 3, and 5, and to make the area identifiers big enough to be read
 * without zooming.  It has also been hacked to omit any edge segment
 * that has an endpoint outside the viewport defined by the lines
 * "X=.05", "X=.95", "Y=.05", and "Y=.95", in the fractional coordinate
 * system.
 */
    c_arseti ("DB",1);
/*
 * Initialize the area map.
 */
    c_arinam (iama,LAMA);
/*
 * Put group 1 edges into the area map.
 */
    c_aredam (iama,perix,periy,5,1, 0,-1);
    c_aredam (iama,g1e1x,g1e1y,9,1, 2, 1);
    c_aredam (iama,g1e2x,g1e2y,5,1, 1, 2);
/*
 * Put group 3 edges into the area map.
 */
    c_aredam (iama,perix,periy,5,3, 0,-1);
    c_aredam (iama,g3e1x,g3e1y,2,3, 1, 2);
    c_aredam (iama,g3e2x,g3e2y,2,3, 2, 3);
    c_aredam (iama,g3e3x,g3e3y,2,3, 3, 4);
    c_aredam (iama,g3e4x,g3e4y,2,3, 4, 5);
    c_aredam (iama,g3e5x,g3e5y,2,3, 5, 6);
    c_aredam (iama,g3e6x,g3e6y,2,3, 6, 7);
    c_aredam (iama,g3e7x,g3e7y,2,3, 7, 8);
    c_aredam (iama,g3e8x,g3e8y,2,3, 8, 9);
    c_aredam (iama,g3e9x,g3e9y,2,3, 9,10);
/*
 * Put group 5 edges into the area map.
 */
    c_aredam (iama,perix,periy,5,5, 0,-1);
    c_aredam (iama,g5e1x,g5e1y,7,5,-1, 0);
/*
 * Scan the area map.
 */
    c_arscam (iama,xcra,ycra,NCRA,iaai,iagi,NGPS,colram);
/*
 * Put a label at the top of the final plot.
 */
    c_plchlq (.5,.98,"THE COLORED AREAS",.014,0.,0.);
/*
 * Advance the frame.
 */
    c_frame();
/* 
 * Test argeti
 */
    c_arseti ("DB",0);
    c_argeti ("DB",&ival);
    printf( "argeti:  ival is supposed to be 0; ival is really %d\n", ival );
/* 
 * Test arsetr and argetr
 */
    c_arsetr ("AL",3.14);
    c_argetr ("AL",&rval);
    printf( "c_arsetr, c_argetr: rval is supposed to be 3.14; rval is really %g\n", rval );
/*
 *  Close GKS
 */
    c_clsgks();
}

#if !defined(cray)
extern struct common1 {
#else
struct common1 {
#endif
    int iad,iau,ilc;
    float rlc;
    int ilm;
    float rlm;
    int ilp;
    float rlp;
    int ibs;
    float rbs,dbs;
    int idb,idc,idi;
} arcomn_;

#if !defined(cray)
extern struct common2 {
#else
struct common2 {
#endif
    float dt;
} arcom1_;

ardbpa_(iama,igip,labl)
int *iama, *igip;
char *labl;
{
/*
 * this is a version of the areas routine ardbpa that has been modified
 * to produce plots for each of edge groups 1, 3, and 5.
 */
    char labm[81];
    int lenl;
/*
 * compute the length of the original label.
 */
    lenl=strlen(labl);
/*
 * do a plot showing the contents of the area map for group 1.
 */
    (void)sprintf( labm, "%s  - GROUP 1", labl );
    (void)ardbpb (iama,1,labm);
    c_frame();
/*
 * do a plot showing the contents of the area map for group 3.
 */
    (void)sprintf( labm, "%s  - GROUP 3", labl );
    (void)ardbpb (iama,3,labm);
    c_frame();
/*
 * do a plot showing the contents of the area map for group 5.
 */
    (void)sprintf( labm, "%s  - GROUP 5", labl );
    (void)ardbpb (iama,5,labm);
    c_frame();
/*
 * do a plot showing the contents of the area map for all groups.
 */
    (void)sprintf( labm, "%s  - ALL GROUPS", labl );
    (void)ardbpb (iama,1,labm);
    (void)ardbpb (iama,3,labm);
    (void)ardbpb (iama,5,labm);
    c_frame();
    return(1);
}

ardbpb (iama,igip,labl)
int *iama, igip;
char *labl;
{
/*
 * this is a version of the areas routine ardbpa that has been modified
 * to use larger characters for the area identifiers and to omit edge
 * segments that are too close to the edge of the plotter frame.  if
 * the common block in areas is changed or if the structure of an area
 * map is changed, this routine may need to be changed to match.
 */
    float xvpl,xvpr,yvpb,yvpt,xwdl,xwdr,ywdb,ywdt,rxcn,rycn,rxco,ryco;
    int lnlg, indx, igid, iail, iair;
/*
 * save the current state of the set call and switch to the fractional
 * coordinate system.
 */
    c_getset (&xvpl,&xvpr,&yvpb,&yvpt,&xwdl,&xwdr,&ywdb,&ywdt,&lnlg);
    c_set (  0.,  1.,  0.,  1.,  0.,  1.,  0.,  1.,   1);
/*
 * put a label at the top of the plot.
 */
    c_plchlq (.5,.98,labl,.014,0.,0.);
/*
 * trace the edges in the area map, drawing arrows as we go.
 */
    arcom1_.dt=0.;
    indx=8;
    rxcn=.5;
    rycn=.5;
    
one:
    rxco=rxcn;
    ryco=rycn;
    rxcn=(float)(iama[indx])/arcomn_.rlc;
    rycn=(float)(iama[indx+1])/arcomn_.rlc;
    
    if (iama[indx+6] != 0) {
        igid=abs(iama[indx+6]);
        if (igid < iama[5]) {
            igid=iama[iama[0]-igid-1]/2;
        }
        else {
            igid=iama[igid-1]/2;
        }
        if (igid == igip) {
            iail=iama[indx+7];
            if (iail > 0) iail=iama[iail-1]/2;
            iair=iama[indx+8];
            if (iair > 0) iair=iama[iair-1]/2;
            ardbda (rxco,ryco,rxcn,rycn,iail,iair);
        }
    }
    else {
        arcom1_.dt=0.;
    }
    if (iama[indx+2] != 0) {
        indx=iama[indx+2];
        goto one;
    }
/*
 * restore the original set call.
 */
    c_set (xvpl,xvpr,yvpb,yvpt,xwdl,xwdr,ywdb,ywdt,lnlg);
}

ardbda (x1,y1,x2,y2,il,ir)
float x1, y1, x2, y2;
int il, ir;
{
/*
 * the routine ardbda is called by ardbpb, above, to draw an arrow from
 * the point (x1,y1) to the point (x2,y2), in the fractional coordinate
 * system.  the left and right area identifiers il and ir are written
 * in the proper positions relative to the arrow.  in order to prevent
 * too many arrowheads from appearing, we keep track of the cumulative
 * distance along edges being drawn (in dt).
 *
 *
 * define character variables required to write the area identifiers.
 */
    char cs[7];
/*
 * define some temporary x and y coordinate arrays, to be used in marking
 * the head and tail of the arrow.
 */
    int i, nc, ic;
    float dx, dy, dp, xc, yc, xl, yl, xr, yr, xt, yt;
    float a, b;
    Gpoint_list fill_area;
/*
 * if either end of the arrow is too close to the edge of the frame,
 * don't draw anything.
 */
    if (x1 < .05 || x1 > .95) return(1);
    if (y1 < .05 || y1 > .95) return(1);
    if (x2 < .05 || x2 > .95) return(1);
    if (y2 < .05 || y2 > .95) return(1);
/*
 * draw the body of the arrow.
 */
    c_plotif(x1,y1,0);
    c_plotif(x2,y2,1);
/*
 * compute the length of the arrow.  if it's zero, quit.
 */
    dx=x2-x1;
    dy=y2-y1;
    dp=sqrt(dx*dx+dy*dy);
    
    if (dp == 0.) return(1);
/*
 * if the area identifiers are in a reasonable range (less than 100,000
 * in absolute value), write them on either side of the arrow.
 */
    if (abs(il) < 100000 && abs(ir) < 100000) {
        
        xc=.5*(x1+x2);
        yc=.5*(y1+y2);
        xl=xc-.025*dy/dp;
        yl=yc+.025*dx/dp;
        (void)sprintf( cs, "%6d", il );
        nc=0;
        for( i=0; i<6; i++ ) {
            ic=cs[i];
            if (ic != ' ') {
                nc=nc+1;
                cs[nc-1] = ic;
            }
        }
        cs[nc] = '\0';
        c_plchlq (xl,yl,cs,.015,0.,0.);
        
        xr=xc+.025*dy/dp;
        yr=yc-.025*dx/dp;
        (void)sprintf( cs, "%6d", ir );
        nc=0;
        for( i=0; i<6; i++ ) {
            ic=cs[i];
            if (ic != ' ') {
                nc=nc+1;
                cs[nc-1] = ic;
            }
        }
        cs[nc] = '\0';
        c_plchlq (xr,yr,cs,.015,0.,0.);
    }
/*
 * Create structure to pass to gfill_area
 */
	fill_area.num_points = 8;
	fill_area.points = (Gpoint *) malloc(16*sizeof(Gfloat));
	if( !fill_area.points ) {
		fprintf( stderr, "ardbda: Not enough memory to create fill area structure\n" );
		gemergency_close_gks();
		exit(1);
	}
/*
 * put a dot at the tail of the arrow.  this will help identify arrows
 * that are drawn tail-to-tail.
 */
    fill_area.points[0].x=x1-.0060;    fill_area.points[0].y=y1;
    fill_area.points[1].x=x1-.0042;    fill_area.points[1].y=y1+.0042;
    fill_area.points[2].x=x1;          fill_area.points[2].y=y1+.0060;
    fill_area.points[3].x=x1+.0042;    fill_area.points[3].y=y1+.0042;
    fill_area.points[4].x=x1+.0060;    fill_area.points[4].y=y1;
    fill_area.points[5].x=x1+.0042;    fill_area.points[5].y=y1-.0042;
    fill_area.points[6].x=x1;          fill_area.points[6].y=y1-.0060;
    fill_area.points[7].x=x1-.0042;    fill_area.points[7].y=y1-.0042;
    gset_fill_colr_ind (1);
    gfill_area    (&fill_area);
/*
 * if the cumulative length of the edge being drawn is too little,
 * quit ...
 */
    arcom1_.dt=arcom1_.dt+dp;
    if(arcom1_.dt <= .008) return(1);
/*
 * ... otherwise, zero the cumulative length ...
 */
    arcom1_.dt=0.;
/*
 * and draw an solid-filled arrowhead.
 */
    b=(dp-.04)/dp;
    a=1.0-b;
    xt=a*x1+b*x2;
    yt=a*y1+b*y2;
    b=(dp-.015)/dp;
    a=1.0-b;
    
	fill_area.num_points = 4;
    fill_area.points[0].x=x2;              fill_area.points[0].y=y2;
    fill_area.points[1].x=xt-.01*dy/dp;    fill_area.points[1].y=yt+.01*dx/dp;
    fill_area.points[2].x=a*x1+b*x2;       fill_area.points[2].y=a*y1+b*y2;
    fill_area.points[3].x=xt+.01*dy/dp;    fill_area.points[3].y=yt-.01*dx/dp;
    gset_fill_colr_ind (1);
    gfill_area (&fill_area);
    if( fill_area.points != NULL ) free(fill_area.points);
    return(1);
}

/*
 * This routine colors the areas defined by the area map.
 */

#ifdef __STDC__
int colram(
    float *xcra,
    float *ycra,
    int *ncra,
    int *iaai,
    int *iagi,
    int *ngps
)
#else
colram (xcra,ycra,ncra,iaai,iagi,ngps)
    float *xcra;
    float *ycra;
    int *ncra;
    int *iaai;
    int *iagi;
    int *ngps;
#endif
{
    int i, iai1, iai3, iai5;
    Gpoint_list fill_area;
/*
 * pick off the individual group identifiers.
 */
    iai1=0;
    iai3=0;
    iai5=0;
    
    for( i = 0; i < *ngps; i++ ) {
        if (iagi[i] == 1) iai1=iaai[i];
        if (iagi[i] == 3) iai3=iaai[i];
        if (iagi[i] == 5) iai5=iaai[i];
    }
/*
 * skip coloring if either of the first two area identifiers is zero or
 * negative or if the final one is negative.
 */
    if (iai1 <= 0 || iai3 <= 0 || iai5 < 0) goto two;
/*
 * otherwise, color the area, using a color index which is obtained by
 * combining the area identifiers for groups 1 and 3.
 */
    gset_fill_colr_ind (10*iai1+iai3);
/*
 * Create structure to pass to gfill_area
 */
    fill_area.num_points = *ncra-1;
    fill_area.points = (Gpoint *) malloc(2*(*ncra-1)*sizeof(Gfloat));
    if( !fill_area.points ) {
        fprintf( stderr, "colram: Not enough memory to create fill area structure\n" );
        gemergency_close_gks();
        exit(1);
    }
    for( i = 0; i < *ncra-1; i++ ) {
        fill_area.points[i].x = xcra[i];
        fill_area.points[i].y = ycra[i];
    }
/*
 * Fill area
 */
    gfill_area (&fill_area);
    if( fill_area.points != NULL ) free(fill_area.points);
two:
    return(1);
}
