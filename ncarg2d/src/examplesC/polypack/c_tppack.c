/*
 *	$Id: c_tppack.c,v 1.3 1994-06-27 17:47:50 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	int ierr;
	extern void tppack();

    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, WSTYPE);
    gactivate_ws(WKID);
    tppack(&ierr);
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

#define NWRK 999
#define NCCP  5
#define NCSP  11

void tppack(ierr)
int *ierr;
{
    int i;
/*
 * PURPOSE                To provide a simple demonstration of the use
 *                        of a couple of the POLYPACK routines.
 *
 * USAGE                  CALL TPPACK (IERR)
 *
 * ARGUMENTS
 *
 * ON OUTPUT              IERR
 *
 *                          an error parameter
 *                          = 0, if the test is successful.
 *
 * I/O                    If the test is successful, the message
 *
 *                          POLYPACK TEST EXECUTED--SEE PLOTS TO CERTIFY
 *
 *                        is written on unit 6.
 *
 * PRECISION              Single.
 *
 * REQUIRED LIBRARY       POLYPACK, SPPS
 * FILES
 *
 * REQUIRED GKS LEVEL     0A
 *
 * LANGUAGE               FORTRAN
 *
 * HISTORY                Written in June, 1994.
 *
 * ALGORITHM              TPPACK defines a simple clip polygon and a
 *                        simple subject polygon, displays them both,
 *                        and uses the POLYPACK routines PPINPO and
 *                        PPINTR to fill the intersection.
 *
 * PORTABILITY            FORTRAN 77
 *
 * Declare arrays in which to define the clip polygon and the subject
 * polygon.
 */
    float xccp[NCCP],yccp[NCCP],xcsp[NCSP],ycsp[NCSP];
/*
 * Declare the required work arrays.
 */
	float  rwrk[NWRK];
	int iwrk[NWRK];
	Gpoint_list line;
/*        EQUIVALENCE (RWRK(1),IWRK(1))*/
/*
 * Tell the compiler that the fill and merge routines for polygons and
 * the fill routine for trapezoids are EXTERNALs, not REALs.
 */
	extern int fillpo(), filltr();
/*
 * Define the clip polygon to be a small square.
 */
	line.points = (Gpoint *)malloc(NCSP*sizeof(Gpoint));
	if( !line.points ) {
		fprintf( stderr, "tppack: Not enough memory to create line structure\n" );
		return;
	}
	line.num_points = NCCP;
	line.points[0].x = line.points[0].y = xccp[0] = yccp[0] = -5.;
	line.points[1].x = xccp[1] = 5.; line.points[1].y = yccp[1] = -5.;
	line.points[2].x = xccp[2] = line.points[2].y = yccp[2] = 5.;
	line.points[3].x = xccp[3] = -5.; line.points[3].y = yccp[3] = 5.;
	line.points[4].x = xccp[4] = line.points[4].y = yccp[4] = -5.;
/*
 * Define the subject polygon to be a diamond with a hole in it.
 */
	xcsp[ 0] =   0. ; ycsp[ 0] =    9.;
	xcsp[ 1] =   0. ; ycsp[ 1] =    6.;
	xcsp[ 2] =   6. ; ycsp[ 2] =    0.;
	xcsp[ 3] =   0. ; ycsp[ 3] =   -6.;
	xcsp[ 4] =  -6. ; ycsp[ 4] =    0.;
	xcsp[ 5] =   0. ; ycsp[ 5] =    6.;
	xcsp[ 6] =   0. ; ycsp[ 6] =    9.;
	xcsp[ 7] =  -9. ; ycsp[ 7] =    0.;
	xcsp[ 8] =   0. ; ycsp[ 8] =   -9.;
	xcsp[ 9] =   9. ; ycsp[ 9] =    0.;
	xcsp[10] =   0. ; ycsp[10] =    9.;
/*
 * Initialize the error flag to zero.
 */
	*ierr=0;
/*
 * Enable solid fill instead of the default hollow fill.
 */
	gset_fill_int_style(GSTYLE_SOLID);
/*
 * Turn off clipping by GKS.
 */
	gset_clip_ind (GIND_NO_CLIP);
/*
 * Put a label on the whole plot.
 */
	c_set    (0.,1.,0.,1.,0.,1.,0.,1.,1);
	c_plchhq (.5,.975,"DEMONSTRATING THE USE OF POLYPACK",.015,0.,0.);
/*
 * In the upper left-hand corner, draw just the clip polygon and the
 * subject polygon.
 */
	c_set (.05,.475,.525,.95,-10.,10.,-10.,10.,1);
	c_plchhq (0.,-9.5,"The subject polygon (hollow diamond) and clip polygon (square).",.008,0.,0.);
	gpolyline(&line);
	line.num_points = NCSP;
	for( i = 0; i < line.num_points; i++ ) {
		line.points[i].x = xcsp[i];
		line.points[i].y = ycsp[i];
	}
	gpolyline(&line);
/*
 * In the upper right-hand corner, fill the difference polygon, using
 * PPDIPO.
 */
	c_set (.525,.95,.525,.95,-10.,10.,-10.,10.,1);
	c_plchhq (0.,-9.5,"The difference (subject polygon minus clip polygon).",.008,0.,0.);
	line.num_points = NCCP;
	for( i = 0; i < line.num_points; i++ ) {
		line.points[i].x = xccp[i];
		line.points[i].y = yccp[i];
	}
	gpolyline(&line);
	line.num_points = NCSP;
	for( i = 0; i < line.num_points; i++ ) {
		line.points[i].x = xcsp[i];
		line.points[i].y = ycsp[i];
	}
	gpolyline(&line);
	c_ppdipo (xccp,yccp,NCCP,xcsp,ycsp,NCSP,rwrk,iwrk,NWRK,fillpo,ierr);
    if (*ierr != 0) {
        printf("POLYPACK ROUTINE PPDIPO RETURNS IERR = %d\n",*ierr );
        return;
    }
/*
 * In the lower left-hand corner, fill the intersection polygon, using
 * PPINPO.
 */
    c_set (.05,.475,.05,.475,-10.,10.,-10.,10.,1);
    c_plchhq (0.,-9.5,"The intersection of the subject and clip polygons.",.008,0.,0.);
	line.num_points = NCCP;
	for( i = 0; i < line.num_points; i++ ) {
		line.points[i].x = xccp[i];
		line.points[i].y = yccp[i];
	}
	gpolyline(&line);
	line.num_points = NCSP;
	for( i = 0; i < line.num_points; i++ ) {
		line.points[i].x = xcsp[i];
		line.points[i].y = ycsp[i];
	}
	gpolyline(&line);
    c_ppinpo (xccp,yccp,NCCP,xcsp,ycsp,NCSP,rwrk,iwrk,NWRK,fillpo,ierr);
    if (*ierr != 0) {
        printf( "POLYPACK ROUTINE PPINPO RETURNS IERR = %d\n",*ierr);
        return;
    }
/*
 * In the lower right-hand corner, fill the union polygon, using PPUNTR.
 */
    c_set (.525,.95,.05,.475,-10.,10.,-10.,10.,1);
    c_plchhq (0.,-9.5,"The union of the subject and clip polygons.",.008,0.,0.);
	line.num_points = NCCP;
	for( i = 0; i < line.num_points; i++ ) {
		line.points[i].x = xccp[i];
		line.points[i].y = yccp[i];
	}
	gpolyline(&line);
	free(line.points);
    c_ppuntr (xccp,yccp,NCCP,xcsp,ycsp,NCSP,rwrk,iwrk,NWRK,filltr,ierr);
    if (*ierr != 0) {
        printf( "POLYPACK ROUTINE PPUNPO RETURNS IERR = %d\n",*ierr);
        return;
    }
/*
 * Advance the frame.
 */
    c_frame();
/*
 * Write the appropriate message.
 */
    printf( "POLYPACK TEST EXECUTED--SEE PLOTS TO CERTIFY\n" );
/*
 * Done.
 */
    return;
}

int fillpo (xcra,ycra,ncra)
float *xcra, *ycra;
int *ncra;
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

int filltr (xcbl,xcbr,ycob,dxle,dxre,ycot)
float *xcbl, *xcbr, *ycob, *dxle, *dxre, *ycot;
{
    Gpoint_list area;
/*
 * This routine fills trapezoids generated by the routines PPDITR,
 * PPINTR, and PPUNTR.
 *
 * If the trapezoid is not degenerate, fill it and outline it.
 */
	if (*ycot > *ycob) {
		area.num_points = 5;
		area.points = (Gpoint *)malloc(area.num_points*sizeof(Gpoint));
		if( !area.points ) {
			fprintf( stderr, "filltr: Not enough memory to create area structure\n" );
			gemergency_close_gks();
			exit(1);
		}
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
		free(area.points);
	}
	return(0);
}

