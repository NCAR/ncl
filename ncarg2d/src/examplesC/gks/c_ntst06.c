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
    Gpoint_list     point_list;
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
    rgbs[0].rgb.red = 0.;    rgbs[0].rgb.green = 0.;    rgbs[0].rgb.blue = .6;
    rgbs[1].rgb.red = 1.;    rgbs[1].rgb.green = 1.;    rgbs[1].rgb.blue = 1.;
    rgbs[2].rgb.red = 1.;    rgbs[2].rgb.green = 1.;    rgbs[2].rgb.blue = 0.;
    rgbs[3].rgb.red = 0.;    rgbs[3].rgb.green = 1.;    rgbs[3].rgb.blue = 0.;
    rgbs[4].rgb.red = 1.;    rgbs[4].rgb.green = 1.;    rgbs[4].rgb.blue = 0.;
    rgbs[5].rgb.red = 0.;    rgbs[5].rgb.green = 1.;    rgbs[5].rgb.blue = 1.;
    rgbs[6].rgb.red = 1.;    rgbs[6].rgb.green = 0.;    rgbs[6].rgb.blue = 1.;
    for( i = 0; i <= 6; i++ ) {
        gset_colr_rep(WKID,i,&rgbs[i]);
    }
    gset_marker_type(1);
    ginq_marker_type(&ierr,&rmty);
    if(ierr != 0 || rmty != 1) {
        printf("GINQ_MARKER_TYPE test UNSUCCESSFUL\n");
    }
    else {
        printf("GINQ_MARKER_TYPE test SUCCESSFUL\n");
    }
/*
 *  Marker 1, dot (fill a large circular dot with markers of type 1).
 */
/*
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
        if( point_list.points ) free(point_list.points);
        point_list.points = (Gpoint *)calloc(1,sizeof(Gpoint));
        if( !point_list.points ) {
            printf( "c_tst6: Not enough memory for point_list.points\n" );
            gemergency_close_gks();
            exit(1);
        }
        point_list.num_points = 1;
        point_list.points->x = x;
        point_list.points->y = y;
        gpolymarker(&point_list);
        gset_marker_colr_ind(1);
        holdx = 2*y0-y;
        point_list.points->y = holdx;
        gpolymarker(&point_list);
        holdx = 2*x0-x;
        holdy = 2*y0-y;
        point_list.points->x = holdx;
        point_list.points->y = holdy;
        gpolymarker(&point_list);
        holdx = 2*x0-x;
        point_list.points->x = holdx;
        point_list.points->y = y;
        gpolymarker(&point_list);
        x = x+ainc;
        if (x < xe) goto m20;
    }
/*
 *   Label the dot.
 */
    c_pcseti("CD",1);
    gset_line_colr_ind(1);
    c_plchhq(x0,y0+r+.05,"MARKER 1 (dot)",.018,0.,0.);
/*
 *  Marker 2, plus (make a plus from the plus markers.)
 */
/*
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
    free(point_list.points);
    point_list.points = (Gpoint *)calloc(ho,sizeof(Gpoint));
    point_list.num_points = ho;
    for( i = 0; i < ho; i++ ) {
        point_list.points[i].x = xm1[i];
        point_list.points[i].y = ym1[i];
    }
    gpolymarker(&point_list);

    for( i = 0; i < ho; i++ ) {
        point_list.points[i].x = xm2[i];
        point_list.points[i].y = ym2[i];
    }
    gpolymarker(&point_list);
/*
 *   label the big plus.
 */
    c_pcseti("CD",1);
    gset_line_colr_ind(1);
    c_plchhq(x0,y0+r+.05,"MARKER 2 (plus)",.018,0.,0.);
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
    free(point_list.points);
    point_list.points = (Gpoint *)calloc(ho,sizeof(Gpoint));
    point_list.num_points = ho;
    for( i = 0; i < ho; i++ ) {
        point_list.points[i].x = xm1[i];
        point_list.points[i].y = ym1[i];
    }
    gpolymarker(&point_list);
    for( i = 0; i < ho; i++ ) {
        point_list.points[i].x = xm2[i];
        point_list.points[i].y = ym2[i];
    }
    gpolymarker(&point_list);
    for( i = 0; i < ho; i++ ) {
        point_list.points[i].x = xm3[i];
        point_list.points[i].y = ym3[i];
    }
    gpolymarker(&point_list);
/*
 *   Label the big asterisk.
 */
    c_pcseti("CD",1);
    gset_line_colr_ind(1);
    c_plchhq(x0,y0+r+.05,"MARKER 3 (asterisk)",.018,0.,0.);
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
    free(point_list.points);
    point_list.points = (Gpoint *)calloc(ho,sizeof(Gpoint));
    point_list.num_points = ho;
    for( i = 0; i < ho; i++ ) {
        point_list.points[i].x = xm1[i];
        point_list.points[i].y = ym1[i];
    }
    gpolymarker(&point_list);
/*
 *   label the big circle.
 */
    c_pcseti("CD",1);
    gset_line_colr_ind(1);
    c_plchhq(x0,y0+r+.05,"Marker 4 (circle)",.018,0.,0.);
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
    free(point_list.points);
    point_list.points = (Gpoint *)calloc(ho,sizeof(Gpoint));
    point_list.num_points = ho;
    for( i = 0; i < ho; i++ ) {
        point_list.points[i].x = xm2[i];
        point_list.points[i].y = ym2[i];
    }
    gpolymarker(&point_list);
    for( i = 0; i < ho; i++ ) {
        point_list.points[i].x = xm3[i];
        point_list.points[i].y = ym3[i];
    }
    gpolymarker(&point_list);
/*
 *   Label the big cross.
 */
    c_pcseti("CD",1);
    gset_line_colr_ind(1);
    c_plchhq(x0,y0+r+.05,"Marker 5 (cross)",.018,0.,0.);
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
    free(point_list.points);
    point_list.points = (Gpoint *)calloc(ho,sizeof(Gpoint));
    point_list.num_points = ho;
    point_list.points[0].x = x0;
    point_list.points[0].y = y0;
    gpolymarker(&point_list);
    c_pcseti("CD",1);
    gset_line_colr_ind(1);
    c_plchhq(x0,y0+.028,"Circle",.015,0.,0.);
    c_plchhq(x0,y0     ,"Scaled",.015,0.,0.);
    c_plchhq(x0,y0-.028,"by 15.",.015,0.,0.);
/*
 *  Label the plot (PLOTCHAR strokes its characters with lines, so the
 *  PLOTCHAR character attributes are controlled by the GKS polyline 
 *  attributes).
 */
    gset_line_colr_ind(2);
    c_pcseti("CD",1);
    gset_linewidth(2.);
    c_plchhq(.5,.915,"Polymarkers",.025,0.,0.);
    c_frame();
/*
 * Test ginq_marker_colr_ind
 */
    gset_marker_colr_ind(3);
    ginq_marker_colr_ind(&err_ind,&colr_ind);
    if( colr_ind != 3) {
        printf( "ntst06:  GINQ_MARKER_COLR_IND test UNSUCCESSFUL\n" );
	}
	else {
        printf( "ntst06:  GINQ_MARKER_COLR_IND test SUCCESSFUL\n" );
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
	else {
        printf( "ntst06:  GINQ_MARKER_SIZE test SUCCESSFUL\n" );
	}
/*
 *  Deactivate and close the workstation, close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}
