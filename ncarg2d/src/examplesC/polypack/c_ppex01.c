/*
 *      $Id: c_ppex01.c,v 1.8 1999-07-27 20:15:06 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define IWTYPE 1
#define WKID   1
#define NWRK 999
#define NCCP  5
#define NCSP  11
#define NWRK  999
/*
 * Define the size of the polyline this subroutine is to generate.
 */
#define NCRA 100

/*
 * The parameter LWRK specifies the length of the workspace to be used
 * by the polyline package.
 */
#define LWRK 100

float xcmp[1000],ycmp[1000];
int ncmp;

main()
{
/*
 * Declare arrays in which to define the clip polygon and the subject
 * polygon.
 */
    float xccp[NCCP],yccp[NCCP],xcsp[NCSP],ycsp[NCSP];
/*
 * Declare the required work arrays.
 */
        float rwrk[NWRK];
        int iwrk[NWRK];
        Gpoint_list line, area;
        Gcolr_rep rgb;
        int i, ierr;
#ifdef NeedFuncProto
        extern int mergpo(float *,float *, int*);
        extern int fillpo(float *,float *, int*);
        extern int filltr(float *,float *,float *,float *,float *,float *);
#else
        extern int mergpo(), fillpo(), filltr();
#endif
/*
 * Tell the compiler that the fill and merge routines for polygons and
 * the fill routine for trapezoids are EXTERNALs, not REALs.
 */
        extern void tstclp();
/*
 * Define the clip polygon to be a small square.
 */
        line.points = (Gpoint *)malloc(NCSP*sizeof(Gpoint));
        if( !line.points ) {
                fprintf( stderr, "ppex01: Not enough memory to create line structure\n" );
        gemergency_close_gks();
        exit(1);
        }
        line.num_points = NCCP;
        xccp[0] = yccp[0] = -5.;
        xccp[1] = 5.; yccp[1] = -5.;
        xccp[2] = yccp[2] = 5.;
        xccp[3] = -5.; yccp[3] = 5.;
        xccp[4] = yccp[4] = -5.;
/*
 * Define the subject polygon to be a diamond with a hole in it.
 */
        line.points[ 0].x = xcsp[ 0] =   0. ; line.points[ 0].y = ycsp[ 0] =    9.;
        line.points[ 1].x = xcsp[ 1] =   0. ; line.points[ 1].y = ycsp[ 1] =    6.;
        line.points[ 2].x = xcsp[ 2] =   6. ; line.points[ 2].y = ycsp[ 2] =    0.;
        line.points[ 3].x = xcsp[ 3] =   0. ; line.points[ 3].y = ycsp[ 3] =   -6.;
        line.points[ 4].x = xcsp[ 4] =  -6. ; line.points[ 4].y = ycsp[ 4] =    0.;
        line.points[ 5].x = xcsp[ 5] =   0. ; line.points[ 5].y = ycsp[ 5] =    6.;
        line.points[ 6].x = xcsp[ 6] =   0. ; line.points[ 6].y = ycsp[ 6] =    9.;
        line.points[ 7].x = xcsp[ 7] =  -9. ; line.points[ 7].y = ycsp[ 7] =    0.;
        line.points[ 8].x = xcsp[ 8] =   0. ; line.points[ 8].y = ycsp[ 8] =   -9.;
        line.points[ 9].x = xcsp[ 9] =   9. ; line.points[ 9].y = ycsp[ 9] =    0.;
        line.points[10].x = xcsp[10] =   0. ; line.points[10].y = ycsp[10] =    9.;
/*
 * Open GKS.
 */
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, IWTYPE);
    gactivate_ws(WKID);
/*
 * Enable solid fill instead of the default hollow fill.
 */
        gset_fill_int_style(GSTYLE_SOLID);
/*
 * Turn off clipping by GKS.
 */
        gset_clip_ind (GIND_NO_CLIP);
/*
 * Tell GKS to use a doubled line width.
 */
        gset_linewidth(2.);
/*
 * Define some colors to use for various purposes.
 */
        rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
        gset_colr_rep(1,0,&rgb);
        rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
        gset_colr_rep(1,1,&rgb);
        rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
        gset_colr_rep(1,2,&rgb);
        rgb.rgb.red = 0.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
        gset_colr_rep(1,3,&rgb);
        rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
        gset_colr_rep(1,4,&rgb);
        rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
        gset_colr_rep(1,5,&rgb);
/*
 * Define the mapping from the user system to the fractional system.
 */
    c_set (.05,.95,.05,.95,-10.,10.,-10.,10.,1);
/*
 * Tell PLOTCHAR to use filled characters from font 25.
 */
    c_pcseti ("FN - FONT NUMBER",25);
/*
 * Fill the polygons representing the difference of the clip polygon
 * and the subject polygon.
 */
    c_plchhq (c_cfux(.5),c_cfuy(.96),"POLYPACK EXAMPLE - FRAME 1",.018,0.,0.);

    c_plchhq (c_cfux(.5),c_cfuy(.93),"Difference polygon formed using routines PPDIPO and FILLPO.",.0175,0.,0.);

    c_plchhq (c_cfux(.5),c_cfuy(.07),"The subject polygon S is outlined in green and the clip polygon C in red.",.0148,0.,0.);

    c_plchhq (c_cfux(.5),c_cfuy(.04),"The difference S-C is filled in white.",.0148,0.,0.);

    c_ppdipo (xccp,yccp,NCCP,xcsp,ycsp,NCSP, rwrk,iwrk,NWRK,fillpo,&ierr);

    if (ierr != 0) printf( "PPDIPO RETURNS IERR = %d\n",ierr );

    gset_line_colr_ind (3);
        line.num_points = NCSP;
    gpolyline(&line);
    gset_line_colr_ind (2);
        line.num_points = NCCP;
        for( i = 0; i < line.num_points; i++ ) {
                line.points[i].x = xccp[i];
                line.points[i].y = yccp[i];
        }
        gpolyline(&line);
    gset_line_colr_ind (1);

        c_frame();
/*
 * Fill the polygons representing the intersection of the clip polygon
 * and the subject polygon.
 */
        c_plchhq (c_cfux(.5),c_cfuy(.96),"POLYPACK EXAMPLE - FRAME 2",.018,0.,0.);

        c_plchhq (c_cfux(.5),c_cfuy(.93), "Intersection polygon formed using routines PPINPO and FILLPO.",.0175,0.,0.);

        c_plchhq (c_cfux(.5),c_cfuy(.07),"The subject polygon S is outlined in green and the clip polygon C in red.",.0148,0.,0.);

        c_plchhq (c_cfux(.5),c_cfuy(.04),"The intersection S:F34:G:F:C is filled in white.", .0148,0.,0.);

        c_ppinpo (xccp,yccp,NCCP,xcsp,ycsp,NCSP, rwrk,iwrk,NWRK,fillpo,&ierr);

    if (ierr != 0) printf( "PPINPO RETURNS IERR = %d\n",ierr );

        line.num_points = NCSP;
        for( i = 0; i < line.num_points; i++ ) {
                line.points[i].x = xcsp[i];
                line.points[i].y = ycsp[i];
        }
    gset_line_colr_ind (3);
        gpolyline(&line);
        line.num_points = NCCP;
        for( i = 0; i < line.num_points; i++ ) {
                line.points[i].x = xccp[i];
                line.points[i].y = yccp[i];
        }
    gset_line_colr_ind (2);
        gpolyline(&line);
    gset_line_colr_ind (1);

    c_frame();
/*
 * Fill the polygons representing the union of the clip polygon
 * and the subject polygon.
 */
    c_plchhq (c_cfux(.5),c_cfuy(.96),"POLYPACK EXAMPLE - FRAME 3",.018,0.,0.);

    c_plchhq (c_cfux(.5),c_cfuy(.93),"Union polygon formed using routines PPUNPO and FILLPO.", .0175,0.,0.);

    c_plchhq (c_cfux(.5),c_cfuy(.07),"The subject polygon S is outlined in green and the clip polygon C in red.",.0148,0.,0.);

    c_plchhq (c_cfux(.5),c_cfuy(.04),"The union S:F34:H:F:C is filled in white.  Using FILLPO results in three errors; compare with frame 6.",.0148,0.,0.);

    c_ppunpo (xccp,yccp,NCCP,xcsp,ycsp,NCSP,rwrk,iwrk,NWRK,fillpo,&ierr);

    if (ierr != 0) printf( "PPUNPO RETURNS IERR = %d\n",ierr );

        line.num_points = NCSP;
        for( i = 0; i < line.num_points; i++ ) {
                line.points[i].x = xcsp[i];
                line.points[i].y = ycsp[i];
        }
    gset_line_colr_ind (3);
        gpolyline(&line);
        line.num_points = NCCP;
        for( i = 0; i < line.num_points; i++ ) {
                line.points[i].x = xccp[i];
                line.points[i].y = yccp[i];
        }
    gset_line_colr_ind (2);
        gpolyline(&line);
    gset_line_colr_ind (1);

    c_frame();
/*
 * Merge the polygons representing the difference of the clip polygon
 * and the subject polygon and fill the result.
 */
    c_plchhq (c_cfux(.5),c_cfuy(.96),"POLYPACK EXAMPLE - FRAME 4",.018,0.,0.);

    c_plchhq (c_cfux(.5),c_cfuy(.93),"Difference polygon formed using routines PPDIPO and MERGPO.",.0175,0.,0.);

    c_plchhq (c_cfux(.5),c_cfuy(.07),"The subject polygon S is outlined in green and the clip polygon C in red.",.0148,0.,0.);

    c_plchhq (c_cfux(.5),c_cfuy(.04),"The difference S-C is filled in white.",.0148,0.,0.);

    ncmp=0;

    c_ppdipo (xccp,yccp,NCCP,xcsp,ycsp,NCSP,rwrk,iwrk,NWRK,mergpo,&ierr);

    if (ierr != 0) printf( "PPDIPO RETURNS IERR = %d\n",ierr );

    if (ncmp == 0) {
        printf ("MERGE POLYGON IS NULL\n" );
    }
    else if(ncmp == 1000) {
        printf ("MERGE POLYGON WAS TOO BIG TO HANDLE\n" );
    }
    else {
        area.num_points = ncmp-1;
        area.points = (Gpoint *)malloc(area.num_points*sizeof(Gpoint));
        if( !area.points ) {
            fprintf( stderr, "ppex01: Not enough memory to create area structure\n" );
            gemergency_close_gks();
            exit(1);
        }
        for( i = 0; i < area.num_points; i++ ) {
            area.points[i].x = xcmp[i+1];
            area.points[i].y = ycmp[i+1];
        }
        gfill_area(&area);
    }
        line.num_points = NCSP;
        for( i = 0; i < line.num_points; i++ ) {
                line.points[i].x = xcsp[i];
                line.points[i].y = ycsp[i];
        }
    gset_line_colr_ind (3);
        gpolyline(&line);
        line.num_points = NCCP;
        for( i = 0; i < line.num_points; i++ ) {
                line.points[i].x = xccp[i];
                line.points[i].y = yccp[i];
        }
    gset_line_colr_ind (2);
        gpolyline(&line);
    gset_line_colr_ind (1);

    c_frame();
/*
 * Merge the polygons representing the intersection of the clip polygon
 * and the subject polygon and fill the result.
 */
    c_plchhq (c_cfux(.5),c_cfuy(.96),"POLYPACK EXAMPLE - FRAME 5",.018,0.,0.);

    c_plchhq (c_cfux(.5),c_cfuy(.93),"Intersection polygon formed using routines PPINPO and MERGPO.",.0175,0.,0.);

    c_plchhq (c_cfux(.5),c_cfuy(.07),"The subject polygon S is outlined in green and the clip polygon C in red.",.0148,0.,0.);

    c_plchhq (c_cfux(.5),c_cfuy(.04),"The intersection S:F34:G:F:C is filled in white.",.0148,0.,0.);

        ncmp=0;

        c_ppinpo (xccp,yccp,NCCP,xcsp,ycsp,NCSP,rwrk,iwrk,NWRK,mergpo,&ierr);

    if (ierr != 0) printf( "PPINPO RETURNS IERR = %d\n",ierr );

    if (ncmp == 0) {
        printf ("MERGE POLYGON IS NULL\n" );
    }
    else if(ncmp == 1000) {
        printf ("MERGE POLYGON WAS TOO BIG TO HANDLE\n" );
    }
    else {
        area.num_points = ncmp-1;
        area.points = (Gpoint *)malloc(area.num_points*sizeof(Gpoint));
        if( !area.points ) {
            fprintf( stderr, "ppex01: Not enough memory to create area structure\n" );
            gemergency_close_gks();
            exit(1);
        }
        for( i = 0; i < area.num_points; i++ ) {
            area.points[i].x = xcmp[i+1];
            area.points[i].y = ycmp[i+1];
        }
        gfill_area(&area);
    }
        line.num_points = NCSP;
        for( i = 0; i < line.num_points; i++ ) {
                line.points[i].x = xcsp[i];
                line.points[i].y = ycsp[i];
        }
    gset_line_colr_ind (3);
        gpolyline(&line);
        line.num_points = NCCP;
        for( i = 0; i < line.num_points; i++ ) {
                line.points[i].x = xccp[i];
                line.points[i].y = yccp[i];
        }
    gset_line_colr_ind (2);
        gpolyline(&line);
    gset_line_colr_ind (1);

    c_frame();
/*
 * Merge the polygons representing the union of the clip polygon
 * and the subject polygon and fill the result.
 */
        c_plchhq (c_cfux(.5),c_cfuy(.96),"POLYPACK EXAMPLE - FRAME 6",.018,0.,0.);

        c_plchhq (c_cfux(.5),c_cfuy(.93), "Union polygon formed using routines PPUNPO and MERGPO.",.0175,0.,0.);

        c_plchhq (c_cfux(.5),c_cfuy(.07),"The subject polygon S is outlined in green and the clip polygon C in red.",.0148,0.,0.);

        c_plchhq (c_cfux(.5),c_cfuy(.04),"The union S:F34:H:F:C is filled in white; using MERGPO fixes the errors seen on frame 3.", .0148,0.,0.);

        ncmp=0;

        c_ppunpo (xccp,yccp,NCCP,xcsp,ycsp,NCSP,rwrk,iwrk,NWRK,mergpo,&ierr);

    if (ierr != 0) printf( "PPUNPO RETURNS IERR = %d\n",ierr );

    if (ncmp == 0) {
        printf ("MERGE POLYGON IS NULL\n" );
    }
    else if(ncmp == 1000) {
        printf ("MERGE POLYGON WAS TOO BIG TO HANDLE\n" );
    }
    else {
        area.num_points = ncmp-1;
        area.points = (Gpoint *)malloc(area.num_points*sizeof(Gpoint));
        if( !area.points ) {
            fprintf( stderr, "ppex01: Not enough memory to create area structure\n" );
            gemergency_close_gks();
            exit(1);
        }
        for( i = 0; i < area.num_points; i++ ) {
            area.points[i].x = xcmp[i+1];
            area.points[i].y = ycmp[i+1];
        }
        gfill_area(&area);
    }
        line.num_points = NCSP;
        for( i = 0; i < line.num_points; i++ ) {
                line.points[i].x = xcsp[i];
                line.points[i].y = ycsp[i];
        }
    gset_line_colr_ind (3);
        gpolyline(&line);
        line.num_points = NCCP;
        for( i = 0; i < line.num_points; i++ ) {
                line.points[i].x = xccp[i];
                line.points[i].y = yccp[i];
        }
    gset_line_colr_ind (2);
        gpolyline(&line);
    gset_line_colr_ind (1);

    c_frame();
/*
 * Fill the trapezoids representing the difference of the clip polygon
 * and the subject polygon.
 */
        c_plchhq (c_cfux(.5),c_cfuy(.96),"POLYPACK EXAMPLE - FRAME 7",.018,0.,0.);

        c_plchhq (c_cfux(.5),c_cfuy(.93),"Difference polygon formed using routines PPDITR and FILLTR.",.0175,0.,0.);

        c_plchhq (c_cfux(.5),c_cfuy(.07),"The subject polygon S is outlined in green, the clip polygon C in red, trapezoid edges in blue.",.0148,0.,0.);

        c_plchhq (c_cfux(.5),c_cfuy(.04),"The trapezoids comprising the difference S-C are filled in white.",.0148,0.,0.);

        gset_line_colr_ind(4);
        c_ppditr (xccp,yccp,NCCP,xcsp,ycsp,NCSP,rwrk,iwrk,NWRK,filltr,&ierr);

    if (ierr != 0) printf( "PPDITR RETURNS IERR = %d\n",ierr );

        line.num_points = NCSP;
        for( i = 0; i < line.num_points; i++ ) {
                line.points[i].x = xcsp[i];
                line.points[i].y = ycsp[i];
        }
    gset_line_colr_ind (3);
        gpolyline(&line);
        line.num_points = NCCP;
        for( i = 0; i < line.num_points; i++ ) {
                line.points[i].x = xccp[i];
                line.points[i].y = yccp[i];
        }
    gset_line_colr_ind (2);
        gpolyline(&line);
    gset_line_colr_ind (1);

    c_frame();
/*
 * Fill the trapezoids representing the intersection of the clip polygon
 * and the subject polygon.
 */
        c_plchhq (c_cfux(.5),c_cfuy(.96),"POLYPACK EXAMPLE - FRAME 8",.018,0.,0.);

        c_plchhq (c_cfux(.5),c_cfuy(.93),"Intersection polygon formed using routines PPINTR and FILLTR.", .0175,0.,0.);

        c_plchhq (c_cfux(.5),c_cfuy(.07),"The subject polygon S is outlined in green, the clip polygon C in red, trapezoid edges in blue.",.0148,0.,0.);

        c_plchhq (c_cfux(.5),c_cfuy(.04),"The trapezoids comprising the intersection S:F34:G:F:C are filled in white.",.0148,0.,0.);

        gset_line_colr_ind(4);
        c_ppintr (xccp,yccp,NCCP,xcsp,ycsp,NCSP,rwrk,iwrk,NWRK,filltr,&ierr);

    if (ierr != 0) printf( "PPINTR RETURNS IERR = %d\n",ierr );

        line.num_points = NCSP;
        for( i = 0; i < line.num_points; i++ ) {
                line.points[i].x = xcsp[i];
                line.points[i].y = ycsp[i];
        }
    gset_line_colr_ind (3);
        gpolyline(&line);
        line.num_points = NCCP;
        for( i = 0; i < line.num_points; i++ ) {
                line.points[i].x = xccp[i];
                line.points[i].y = yccp[i];
        }
    gset_line_colr_ind (2);
        gpolyline(&line);
    gset_line_colr_ind (1);

    c_frame();
/*
 * Fill the trapezoids representing the union of the clip polygon
 * and the subject polygon.
 */
        c_plchhq (c_cfux(.5),c_cfuy(.96),"POLYPACK EXAMPLE - FRAME 9",.018,0.,0.);

        c_plchhq (c_cfux(.5),c_cfuy(.93),"Union polygon formed using routines PPUNTR and FILLTR.",.0175,0.,0.);

        c_plchhq (c_cfux(.5),c_cfuy(.07),"The subject polygon S is outlined in green, the clip polygon C in red, trapezoid edges in blue.",.0148,0.,0.);

        c_plchhq (c_cfux(.5),c_cfuy(.04),"The trapezoids comprising the union S:F34:H:F:C are filled in white.",.0148,0.,0.);

        gset_line_colr_ind(4);
        c_ppuntr (xccp,yccp,NCCP,xcsp,ycsp,NCSP,rwrk,iwrk,NWRK,filltr,&ierr);

    if (ierr != 0) printf( "PPUNTR RETURNS IERR = %d\n",ierr );

        line.num_points = NCSP;
        for( i = 0; i < line.num_points; i++ ) {
                line.points[i].x = xcsp[i];
                line.points[i].y = ycsp[i];
        }
    gset_line_colr_ind (3);
        gpolyline(&line);
        line.num_points = NCCP;
        for( i = 0; i < line.num_points; i++ ) {
                line.points[i].x = xccp[i];
                line.points[i].y = yccp[i];
        }
    gset_line_colr_ind (2);
        gpolyline(&line);
    gset_line_colr_ind (1);

    c_frame();
        free(line.points);
/*
 * Do one more test, of polyline clipping.
 */
        tstclp();
/*
 * Close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

void tstclp()
{
        extern float ppfran();
/*
 * Define some arrays in which to put the coordinates of lines.
 */
        float xcra[NCRA],ycra[NCRA];
        float frct, xwdl, xwdr, ywdb, ywdt;
        int ierr,j;
/*
 * The array RWRK is a workspace array required by the polyline clipper.
 */
        float rwrk[LWRK];
/*
 * The routine DRAWEM is called by the polyline clipping routine.
 */
#ifdef NeedFuncProto
        extern int drawem(float *,float *,int *);
#else
        extern int drawem();
#endif
/*
 * Initialize the polyline color index and the line width scale factor.
 */
        gset_line_colr_ind(1);
        gset_linewidth (1.);
/*
 * Call SET in such a way that we can use fractional coordinates.
 */
        c_set (0.,1.,0.,1.,-.15,1.15,-.15,1.15,1);
/*
 * Create a random polyline.
 */
        for( j = 0; j < NCRA; j++ ) {
                xcra[j] = ppfran();
                ycra[j] = ppfran();
        }
/*
 * Draw the polyline twice, once directly and once using the clipper, a
 * different color, and a greater line width.
 */
    c_plchhq (c_cfux(.5),c_cfuy(.96),"POLYPACK EXAMPLE - FRAME 10",.018,0.,0.);

        c_plchhq (c_cfux(.5),c_cfuy(.93),"Demonstrating the use of PPPLCL for clipping polylines.",.0175,0.,0.);

        c_plchhq (c_cfux(.5),c_cfuy(.07),"The clipping rectangle is outlined in magenta.",.0148,0.,0.);

        c_plchhq (c_cfux(.5),c_cfuy(.04),"The polyline is first drawn in white and then again in red using PPPLCL.",.0148,0.,0.);

        frct = .25;
        xwdl = frct;
        xwdr = 1.-frct;
        ywdb = frct;
        ywdt = 1.-frct;

        gset_line_colr_ind(1);
        gset_linewidth (2.);
        c_curve  (xcra,ycra,NCRA);

        gset_line_colr_ind(2);
        gset_linewidth (4.);
        c_ppplcl (xwdl,xwdr,ywdb,ywdt,xcra,ycra,NCRA,rwrk,LWRK,drawem,&ierr);

        if (ierr != 0) {
                printf("ERROR IN PPPLCL, IERR = %d\n", ierr);
        }

        gset_line_colr_ind(5);
        gset_linewidth (2.);
        c_line   (xwdl,ywdb,xwdr,ywdb);
        c_line   (xwdr,ywdb,xwdr,ywdt);
        c_line   (xwdr,ywdt,xwdl,ywdt);
        c_line   (xwdl,ywdt,xwdl,ywdb);
/*
 * Advance the frame.
 */
        c_frame();
/*
 * Done.
 */
        return;
}

int fillpo
#ifdef NeedFuncProto
(float *xcra,float *ycra,int *ncra)
#else
(xcra,ycra,ncra)
float *xcra, *ycra;
int *ncra;
#endif
{
        int i;
/*
 * This routine processes polygons generated by the routines PPDIPO,
 * PPINPO, and PPUNPO.
 *
 * Fill the polygon.
 */
    Gpoint_list area;
        area.num_points = *ncra-1;
        area.points = (Gpoint *)malloc(area.num_points*sizeof(Gpoint));
        if( !area.points ) {
                fprintf( stderr, "fillpo: Not enough memory to create area structure\n" );
                gemergency_close_gks();
                exit(1);
        }
        for( i = 0; i < area.num_points; i++ ) {
                area.points[i].x = xcra[i];
                area.points[i].y = ycra[i];
        }
        gfill_area(&area);
        free(area.points);
/*
 * Done.
 */
        return(0);
}

int mergpo
#ifdef NeedFuncProto
(float *xcra,float *ycra,int *ncra)
#else
(xcra,ycra,ncra)
float *xcra, *ycra;
int *ncra;
#endif
{
        int i;
/*
 * This routine merges the polygons generated by one of the routines
 * PPDIPO, PPINPO, and PPUNPO into a single polygon with holes.
 *
 * Copy the coordinates of the latest polygon into the merge polygon
 * coordinate arrays and, if the polygon is not the first of the group,
 * repeat the first point of the first polygon.  (Actually, the code
 * below does something a little more complicated: if necessary, it
 * interpolates points to ensure that the connecting lines between
 * polygons consist of horizontal and/or vertical steps; this tends
 * to prevent problems caused by deficiencies in the fill algorithms
 * on some devices.)
 */
        int icra, ntmp;

        ntmp=ncmp;

        if ((ntmp + *ncra + 4) <= 999) {
                if (ncmp != 0) {
            if (xcmp[ntmp] != xcra[0] && ycmp[ntmp] != ycra[0]) {
                                if (ycmp[ntmp] < ycra[0]) {
                                        ntmp=ntmp+1;
                                        xcmp[ntmp]=xcra[0];
                                        ycmp[ntmp]=ycmp[ntmp-1];
                                }
                else {
                    ntmp=ntmp+1;
                    xcmp[ntmp]=xcmp[ntmp-1];
                    ycmp[ntmp]=ycra[0];
                }
            }
                        ntmp=ntmp+1;
                        xcmp[ntmp]=xcra[0];
                        ycmp[ntmp]=ycra[0];
        }
        for( icra = 1; icra <= *ncra; icra++ ) {
            xcmp[ntmp+icra]=xcra[icra-1];
            ycmp[ntmp+icra]=ycra[icra-1];
        }
        ntmp=ntmp + *ncra;
        if (ncmp != 0) {
            if (xcmp[ntmp] != xcmp[1] && ycmp[ntmp] != ycmp[1]) {
                if (ycmp[ntmp] < ycmp[1]) {
                    ntmp=ntmp+1;
                    xcmp[ntmp]=xcmp[1];
                    ycmp[ntmp]=ycmp[ntmp-1];
                }
                else {
                    ntmp=ntmp+1;
                    xcmp[ntmp]=xcmp[ntmp-1];
                    ycmp[ntmp]=ycmp[1];
                }
            }
            ntmp=ntmp+1;
            xcmp[ntmp]=xcmp[1];
            ycmp[ntmp]=ycmp[1];
        }
    }
        else {
                ntmp=1000;
        }
    ncmp=ntmp;
/*
 * done.
 */
    return(0);
}

int filltr
#ifdef NeedFuncProto
(float *xcbl,float *xcbr,float *ycob,float *dxle,float *dxre,float *ycot)
#else
(xcbl,xcbr,ycob,dxle,dxre,ycot)
float *xcbl, *xcbr, *ycob, *dxle, *dxre, *ycot;
#endif
{
    Gpoint_list area;
/*
 * This routine fills trapezoids generated by the routines PPDITR,
 * PPINTR, and PPUNTR.
 *
 * If the trapezoid is not degenerate, fill it and outline it.
 */
        area.num_points = 5;
        area.points = (Gpoint *)malloc(area.num_points*sizeof(Gpoint));
        if( !area.points ) {
                fprintf( stderr, "filltr: Not enough memory to create area structure\n" );
                gemergency_close_gks();
                exit(1);
        }
        if (*ycot > *ycob) {
                area.points[0].x = *xcbl;
                area.points[0].y = *ycob;
                area.points[1].x = *xcbr;
                area.points[1].y = *ycob;
                area.points[2].x = *xcbr + *dxre * (*ycot - *ycob);
                area.points[2].y = *ycot;
                area.points[3].x = *xcbl + *dxle * (*ycot - *ycob);
                area.points[3].y = *ycot;
                area.points[4].x = *xcbl;
                area.points[4].y = *ycob;
                area.num_points = 4;
                gfill_area(&area);
                area.num_points = 5;
                gpolyline(&area);
        }
        free(area.points);
        return(0);
}

float ppfran()
{
        extern double floor(double x);
        static double x = 2.718281828459045;
        double y, z;
        y = 9821.0*x+.211327;
        z = floor(y);
        x = y - z;
        return((float)x);
}

int drawem
#ifdef NeedFuncProto
(float *xcra,float *ycra,int *ncra)
#else
(xcra,ycra,ncra)
float *xcra, *ycra;
int *ncra;
#endif
{
/*
 * Draw the polyline fragment defined by the arguments.
 */
        c_curve (xcra,ycra,*ncra);

        return(0);
}
