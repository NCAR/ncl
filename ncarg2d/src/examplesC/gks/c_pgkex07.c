#include <math.h>
#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define max(x,y) ((x) > (y) ? (x) : (y))

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
/*
 *  Illustrate polymarkers.
 */
    Gpoint_list     markers;
    Gint err_ind, colr_ind;
    Gdouble size;
    Gcolr_rep       rgbs[7];
    int id = 50;
    int k,i,j,idx;
    int ho,rmty,ierr;
    float hld1,hld2,hld3;
    float cs,sn,x,xe,radinc,p;
    float  xm1[50],ym1[50],xm2[50],ym2[50],xm3[50],ym3[50];
    float x0,y0,r,jl,ainc,y;
    float holdx,holdy;
    
    gopen_gks ("stdout",0);
    gopen_ws( WKID, NULL, WSTYPE);
    gactivate_ws( WKID );
/*
 *  Define the necessary color indices.
 */
    rgbs[0].rgb.red = 1.;    rgbs[0].rgb.green = 1.;    rgbs[0].rgb.blue = 1.;
    rgbs[1].rgb.red = 0.;    rgbs[1].rgb.green = 0.;    rgbs[1].rgb.blue = 0.;
    rgbs[2].rgb.red = 1.;    rgbs[2].rgb.green = 0.;    rgbs[2].rgb.blue = 0.;
    rgbs[3].rgb.red = 0.;    rgbs[3].rgb.green = 0.;    rgbs[3].rgb.blue = 1.;
    rgbs[4].rgb.red = 0.;    rgbs[4].rgb.green = 1.;    rgbs[4].rgb.blue = 0.;
    rgbs[5].rgb.red = 0.4;    rgbs[5].rgb.green = 0.;    rgbs[5].rgb.blue = .4;
    rgbs[6].rgb.red = 1.;    rgbs[6].rgb.green = 0.;    rgbs[6].rgb.blue = 0.;
    for( i = 0; i <= 6; i++ ) {
        gset_colr_rep(WKID,i,&rgbs[i]);
    }
    gset_marker_type(1);
    ginq_marker_type(&ierr,&rmty);
    if(ierr != 0 || rmty != 1) {
        printf("GINQ_MARKER_TYPE test UNSUCCESSFUL\n");
    }
/*
 *  Marker 1, dot (fill a large circular dot with markers of type 1).
 *
 *   Position and radius of the dot outline.
 */
    x0 = .5;
    y0 = .7;
    r  = .08;
    jl = 17;
    ainc = r/jl;
    gset_marker_type(1);
    
    for(j=0;j<=jl;j++){
        y = y0+(j)*ainc;
        hld1 = r*r-(y-y0)*(y-y0);
        hld2 = max(hld1,0.);
        hld3 = (float)sqrt(hld2);
        xe = x0+hld3;
        x = x0;
m20:     
/*
 *  Fill the circle with dot markers using symmetries.
 */
        if( markers.points ) free(markers.points);
        markers.points = (Gpoint *)malloc(sizeof(Gpoint));
        if( !markers.points ) {
            printf( "c_pgkex07: Not enough memory for markers\n" );
            gemergency_close_gks();
            exit(1);
        }
        markers.num_points = 1;
        markers.points->x = x;
        markers.points->y = y;
        gpolymarker(&markers);
        gset_marker_colr_ind(1);
        holdx = 2*y0-y;
        markers.points->y = holdx;
        gpolymarker(&markers);
        holdx = 2*x0-x;
        holdy = 2*y0-y;
        markers.points->x = holdx;
        markers.points->y = holdy;
        gpolymarker(&markers);
        holdx = 2*x0-x;
        markers.points->x = holdx;
        markers.points->y = y;
        gpolymarker(&markers);
        x = x+ainc;
        if (x < xe) goto m20;
    }
/*
 *   Label the dot.
 */
    c_pcseti("FN",21);
    c_pcseti("CC",5);
    c_plchhq(x0,y0+r+.05,"MARKER 1 (dot)",.022,0.,0.);
/*
 *  Marker 2, plus (make a plus from the plus markers.)
 *
 *   Center of big plus.
 */
    x0 = .83;
    y0 = .5;
    r  = .1;
    jl = 7;
    ainc = r/jl;
    for(k = 0;k<=2*jl;k++){
        j = k-jl;
        y = y0+(j)*ainc;
        idx = j+jl;
        xm1[idx] = x0;
        ym1[idx] = y;
        x = x0+(j)*ainc;
        xm2[idx] = x;
        ym2[idx] = y0;
    }
    gset_marker_type(2);
    gset_marker_colr_ind(3);
/*
 *  Put plus markers along the two axes of the big plus.
 */
    ho = 2*jl;
    free(markers.points);
    markers.num_points = ho;
    markers.points = (Gpoint *)malloc(markers.num_points*sizeof(Gpoint));
    for( i = 0; i < ho; i++ ) {
        markers.points[i].x = xm1[i];
        markers.points[i].y = ym1[i];
    }
    gpolymarker(&markers);

    for( i = 0; i < ho; i++ ) {
        markers.points[i].x = xm2[i];
        markers.points[i].y = ym2[i];
    }
    gpolymarker(&markers);
/*
 *   label the big plus.
 */
    c_pcseti("FN",21);
    c_pcseti("CC",5);
    gset_line_colr_ind(1);
    c_plchhq(x0,y0+r+.05,"MARKER 2 (plus)",.022,0.,0.);
/*
 *  Marker 3, asterisk (make an asterisk from the asterisk markers.)
 */
    x0 = .7;
    y0 = .15;
    r  = .1;
    jl = 5;
    ainc = r/(jl);
    for(k = 0;k<=2*jl;k++){
        j = k-jl;
        y = y0+(j)*ainc;
        idx = j+jl;
        xm1[idx] = x0;
        ym1[idx] = y;
        p = 0.5*sqrt(2.)*(y-y0);
        if (y >  0.){
            xm2[idx] = x0+p;
            ym2[idx] = y0+p;
            xm3[idx] = x0-p;
            ym3[idx] = y0+p;
        }else{
            xm2[idx] = x0-p;
            ym2[idx] = y0-p;
            xm3[idx] = x0+p;
            ym3[idx] = y0-p;
        }
    }
    gset_marker_type(3);
    gset_marker_colr_ind(4);
/*
 * Put asterisk markers along the axes of the big asterisk.
 */
    ho = 2*jl;
    free(markers.points);
    markers.num_points = ho;
    markers.points = (Gpoint *)malloc(markers.num_points*sizeof(Gpoint));
    for( i = 0; i < ho; i++ ) {
        markers.points[i].x = xm1[i];
        markers.points[i].y = ym1[i];
    }
    gpolymarker(&markers);
    for( i = 0; i < ho; i++ ) {
        markers.points[i].x = xm2[i];
        markers.points[i].y = ym2[i];
    }
    gpolymarker(&markers);
    for( i = 0; i < ho; i++ ) {
        markers.points[i].x = xm3[i];
        markers.points[i].y = ym3[i];
    }
    gpolymarker(&markers);
/*
 *   Label the big asterisk.
 */
    c_pcseti("FN",21);
    c_pcseti("CC",5);
    c_plchhq(x0,y0+r+.05,"MARKER 3 (asterisk)",.022,0.,0.);
/*
 *  Marker 4, circle (make a big circle from the circle markers.)
 */
    x0 = .3;
    y0 = .15;
    r  = .1;
    jl = 50;
    radinc = 2.*3.1415926/(jl);
    for(j=1;j<=jl;j++){
        cs = (float)cos((j)*radinc);
        sn = (float)sin((j)*radinc);
        x = x0+r*cs;
        y = y0+r*sn;
        xm1[j-1] = x;
        ym1[j-1] = y;
    }
    gset_marker_type(4);
    gset_marker_colr_ind(5);
    ho = jl;
    free(markers.points);
    markers.num_points = ho;
    markers.points = (Gpoint *)malloc(markers.num_points*sizeof(Gpoint));
    for( i = 0; i < ho; i++ ) {
        markers.points[i].x = xm1[i];
        markers.points[i].y = ym1[i];
    }
    gpolymarker(&markers);
/*
 *   label the big circle.
 */
    c_pcseti("FN",21);
    c_pcseti("CC",5);
    c_plchhq(x0,y0+r+.05,"Marker 4 (circle)",.022,0.,0.);
/*
 *  Marker 5, cross (make a big cross from the cross markers.)
 */
    x0 = .17;
    y0 = .5;
    r  = .1;
    jl = 5;
    ainc = r/(jl);
    for(k=0;k<2*jl;k++){
        j = k-jl;
        y = y0+(j)*ainc;
        idx = j+jl;
        hld1 = (float) sqrt(2.);
        p = 0.5*hld1*(y-y0);
        if (y >= 0.){
            xm2[idx] = x0+p;
            ym2[idx] = y0+p;
            xm3[idx] = x0-p;
            ym3[idx] = y0+p;
        }else{
            xm2[idx] = x0-p;
            ym2[idx] = y0-p;
            xm3[idx] = x0+p;
            ym3[idx] = y0-p;
        }
    } 
    gset_marker_type(5);
    gset_marker_colr_ind(6);
/*
 *  Plot cross markers along the axes of the big cross.
 */
    ho = 2*jl;
    free(markers.points);
    markers.num_points = ho;
    markers.points = (Gpoint *)malloc(markers.num_points*sizeof(Gpoint));
    for( i = 0; i < ho; i++ ) {
        markers.points[i].x = xm2[i];
        markers.points[i].y = ym2[i];
    }
    gpolymarker(&markers);
    for( i = 0; i < ho; i++ ) {
        markers.points[i].x = xm3[i];
        markers.points[i].y = ym3[i];
    }
    gpolymarker(&markers);
/*
 *   Label the big cross.
 */
    c_pcseti("FN",21);
    c_pcseti("CC",5);
    c_plchhq(x0,y0+r+.05,"Marker 5 (cross)",.022,0.,0.);
/*
 *  Draw a big circle in the center by applying a large marker size
 *  scale factor to the circle marker.
 */
    x0 = .5;
    y0 = .46;
    gset_marker_type(4);
    gset_marker_colr_ind(5);
    gset_marker_size(15.);
    ho = 1;
    free(markers.points);
    markers.num_points = ho;
    markers.points = (Gpoint *)malloc(markers.num_points*sizeof(Gpoint));
    markers.points[0].x = x0;
    markers.points[0].y = y0;
    gpolymarker(&markers);
    if( markers.points ) free(markers.points);
    c_pcseti("FN",21);
    c_pcseti("CC",5);
    c_plchhq(x0,y0+.035,"Circle",.021,0.,0.);
    c_plchhq(x0,y0     ,"Scaled",.021,0.,0.);
    c_plchhq(x0,y0-.035,"by 15.",.021,0.,0.);
/*
 *  Label the plot (PLOTCHAR strokes its characters with lines, so the
 *  PLOTCHAR character attributes are controlled by the GKS polyline 
 *  attributes).
 */
    c_pcseti("FN",25);
    c_pcseti("CC",5);
    c_plchhq(.5,.915,"Polymarkers",.035,0.,0.);
    c_frame();
/*
 * Test ginq_marker_colr_ind
 */
    gset_marker_colr_ind(3);
    ginq_marker_colr_ind(&err_ind,&colr_ind);
    if( colr_ind != 3) {
        printf( "ntst06:  GINQ_MARKER_COLR_IND test UNSUCCESSFUL\n" );
	}
/*
 * Test ginq_marker_size
 */
    gset_marker_size(15.5);
    size = 0.0;
    ginq_marker_size(&err_ind,&size);
    if( size != 15.5) {
        printf( "ntst06:  GINQ_MARKER_SIZE test UNSUCCESSFUL\n" );
        printf( "size should be 15.5, size is really %g\n", size );
	}
/*
 *  Deactivate and close the workstation, close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}
