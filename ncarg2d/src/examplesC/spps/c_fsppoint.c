/*
 *	$Id: c_fsppoint.c,v 1.1 1994-08-03 14:42:31 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define NAMA 5000
#define mod(i,j)   ((i) % (j))

main()
{
    int ierr;
	extern void tareas();
/*
 * Open GKS, open and activate a workstation.
 */
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, WSTYPE);
    gactivate_ws(WKID);
/* 
 * Invoke demo driver
 */
    tareas(&ierr);
/*
 * Deactivate and close workstation, close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}


void tareas (ierror)
int *ierror;
{
    int i,j,ing,isu,ie;
    float ang;
    float rad,xcd,ycd;
    float xcn,ycn;
    int icf,nai,itm,it1,it2;
    int ncd, mcs, mai;
/*
 * PURPOSE                To provide a simple demonstration of the use
 *                        of AREAS.
 *
 * USAGE                  tareas (ierror)
 *
 * ARGUMENTS
 *
 * ON INPUT               WKID
 *                          A workstation id number
 *
 * ON OUTPUT              IERROR
 *
 *                          an error parameter
 *                          = 0, if the test is successful,
 *                          = 1, otherwise
 *
 * I/O                    If the test is successful, the message
 *
 *                          AREAS TEST EXECUTED--SEE PLOTS TO CERTIFY
 *
 *                        is written on unit 6.
 *
 * PRECISION              Single.
 *
 * REQUIRED LIBRARY       AREAS, SPPS
 * FILES
 *
 * REQUIRED GKS LEVEL     0A
 *
 * LANGUAGE               FORTRAN
 *
 * HISTORY                Written in JUNE, 1987.
 *
 * ALGORITHM              TAREAS constructs and colors a very simple
 *                        picture illustrating the use of all of the
 *                        routines in the package.
 *
 * PORTABILITY            FORTRAN 77
 *
 * Define an array in which to construct the area map.
 */
	int iam[NAMA];
/*
 * Define the arrays needed for edge-coordinate data.
 */
	float xca[73],yca[73];
/*
 * Dimension the arrays needed by ARSCAM and ARDRLN for X/Y coordinates.
 */
	float xcs[150],ycs[150];
/*
 * Dimension the arrays needed by ARSCAM and ARDRLN for area and group
 * identifiers.
 */
	int iai[2],iag[2];
/*
 * Define the RGB color triples needed below.
 */
	Gcolr_rep rgb[16];
/*
 * Define a set of indices to order the colors.
 */
int ioc[15] = {6,2,5,12,10,11,1,3,4,8,9,7,13,14,15};

/*
 * Define an array for GKS aspect source flags.
 */
	Gasfs if1;
/*
 * Declare the routine which will color the areas.
 */
	extern int colram();
/*
 * Declare the constant for converting from degrees to radians.
 */
	float dtr = .017453292519943;
/*
 * Declare the routine which will draw lines over the circle.
 */
	extern int colrln();

	rgb[0].rgb.red = 1.00; rgb[0].rgb.green = 1.00; rgb[0].rgb.blue = 1.00;
	rgb[1].rgb.red = 0.10; rgb[1].rgb.green = 0.10; rgb[1].rgb.blue = 0.10;
	rgb[2].rgb.red = 0.75; rgb[2].rgb.green = 0.50; rgb[2].rgb.blue = 1.00;
	rgb[3].rgb.red = 0.50; rgb[3].rgb.green = 0.00; rgb[3].rgb.blue = 1.00;
	rgb[4].rgb.red = 0.00; rgb[4].rgb.green = 0.00; rgb[4].rgb.blue = 1.00;
	rgb[5].rgb.red = 0.00; rgb[5].rgb.green = 0.50; rgb[5].rgb.blue = 1.00;
	rgb[6].rgb.red = 0.00; rgb[6].rgb.green = 1.00; rgb[6].rgb.blue = 1.00;
	rgb[7].rgb.red = 0.00; rgb[7].rgb.green = 1.00; rgb[7].rgb.blue = 0.60;
	rgb[8].rgb.red = 0.00; rgb[8].rgb.green = 1.00; rgb[8].rgb.blue = 0.00;
	rgb[9].rgb.red = 0.70; rgb[9].rgb.green = 1.00; rgb[9].rgb.blue = 0.00;
	rgb[10].rgb.red = 1.00; rgb[10].rgb.green = 1.00; rgb[10].rgb.blue = 0.00;
	rgb[11].rgb.red = 1.00;rgb[11].rgb.green = 0.75;rgb[11].rgb.blue = 0.00;
	rgb[12].rgb.red = 1.00;rgb[12].rgb.green = 0.38;rgb[12].rgb.blue = 0.38;
	rgb[13].rgb.red = 1.00;rgb[13].rgb.green = 0.00;rgb[13].rgb.blue = 0.38;
	rgb[14].rgb.red = 1.00;rgb[14].rgb.green = 0.00;rgb[14].rgb.blue = 0.00;
	rgb[15].rgb.red = 0.00;rgb[15].rgb.green = 1.00;rgb[15].rgb.blue = 0.00;
/*
 * Set the aspect source flags for FILL AREA INTERIOR STYLE and for
 * FILL AREA STYLE INDEX to "individual".
 */
    ginq_asfs(&ie,&if1);
    if1.fill_int_style=1;
    if1.fill_style_ind=1;
    gset_asfs(&if1);
/*
 * Force solid fill.
 */
    gset_fill_int_style(GSTYLE_SOLID);
/*
 * Define background color
 */
	gset_colr_rep(WKID,0,&rgb[0]);
/*
 * Define 16 different color indices.
 */
	for( j = 1; j <= 15; j++ ) {
		i = ioc[j-1];
		gset_colr_rep(WKID,j,&rgb[i]);
	}
/*
 * Initialize the area map.
 */
	c_arinam (iam,NAMA);
/*
 * Add edges to the area map.
 */
	c_set (.01,.99,.01,.99,-1.,1.,-1.,1.,1);
/*
 * First, define a circle, using group 1 edges.  After this step, the
 * area inside the circle has area identifier zero and the area outside
 * has area identifier -1.
 */
	for( ing=1; ing <= 73; ing++ ) {
		ang=dtr*(float)(5*(ing-1));
		xca[ing-1]=cos(ang);
		yca[ing-1]=sin(ang);
	}
	c_aredam (iam,xca,yca,73,1,0,-1);
/*
 * Add lines splitting the circle into wedges.  The area identifiers
 * for the wedges are added to the area map with this step.
 */
	xca[0]=0.;
	yca[0]=0.;
	for( ing=1; ing <= 15; ing++ ) {
		ang=dtr*(float)(24*(ing-1));
		xca[1]=cos(ang);
		yca[1]=sin(ang);
		c_aredam (iam,xca,yca,2,1,ing,mod(ing+13,15)+1);
	}
/*
 * Now, put in another, smaller, off-center circle, using a group 2
 * edge.  The interior of the circle has area identifier 1 and the
 * exterior of the circle has group identifier 2.
 */
	for( ing=1; ing <= 73; ing++ ) {
		ang=dtr*(float)(5*(ing-1));
		xca[ing-1]=.25+.5*cos(ang);
		yca[ing-1]=.25+.5*sin(ang);
	}
	c_aredam (iam,xca,yca,73,2,1,2);
/*
 * Pre-process the area map.
 */
	c_arpram (iam,0,0,0);
/*
 * Compute and print the amount of space used in the area map.
 */
	isu=NAMA-(iam[5]-iam[4]-1);
	printf( "SPACE USED IN AREA MAP IS %d\n", isu );
/*
 * Color the areas defined.
 */
	c_arscam (iam,xcs,ycs,150,iai,iag,2,colram);
/*
 * In contrasting colors, draw three stars on the plot.
 */
	for( i = 1; i <= 3; i++ ) {
		if (i == 1) {
			xcn = -.5;
			ycn = .5;
		}
		else if (i == 2) {
			xcn = -.5;
			ycn = -.5;
		}
		else if (i == 3) {
			xcn = .5;
			ycn = -.5;
		}
		xca[0]=xcn+.25*cos( 162.*dtr);
		yca[0]=ycn+.25*sin( 162.*dtr);
		xca[1]=xcn+.25*cos(  18.*dtr);
		yca[1]=ycn+.25*sin(  18.*dtr);
		xca[2]=xcn+.25*cos(-126.*dtr);
		yca[2]=ycn+.25*sin(-126.*dtr);
		xca[3]=xcn+.25*cos(  90.*dtr);
		yca[3]=ycn+.25*sin(  90.*dtr);
		xca[4]=xcn+.25*cos( -54.*dtr);
		yca[4]=ycn+.25*sin( -54.*dtr);
		xca[5]=xcn+.25*cos( 162.*dtr);
		yca[5]=ycn+.25*sin( 162.*dtr);
		c_ardrln (iam,xca,yca,6,xcs,ycs,150,iai,iag,2,colrln);
	}
/*     
 * Draw a spiral of points in the blanked-out circle, using the colors
 *  from edge group 1.
 */     
	icf=1;
	for( ing=1; ing <= 1500; ing++ ) {
		rad=(float)(ing)/1000.;
		ang=dtr*(float)(ing-1);
		xcd=.25+.5*rad*cos(ang);
		ycd=.25+.5*rad*sin(ang);
		c_argtai (iam,xcd,ycd,iai,iag,2,&nai,icf);
		itm=1;
		for( i = 0; i < nai; i++ ) {
			if (iai[i] < 0) itm=0;
		}
		if (itm != 0) {
			it1=0;
			it2=0;
			for( i = 0; i < nai; i++ ) {
				if (iag[i] == 1) it1=iai[i];
				if (iag[i] == 2) it2=iai[i];
			}
			if (it1 > 0 && it2 == 1) {
/*
 * Flush the polyline buffers and set polyline color index.
 */
				c_sflush();
				gset_line_colr_ind(it1);
				c_point (xcd,ycd);
			}
		}
		icf=0;
	}
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Done.
 */
	*ierror=0;
	printf("AREAS TEST EXECUTED--SEE PLOTS TO CERTIFY\n");
	return;
}

int colram(
    float *xcs,
    float *ycs,
    int *ncs,
    int *iai,
    int *iag,
    int *nai
)
{
	int i, itm, it1, it2;
	Gpoint_list fill_area;

	itm=1;
	for( i = 0; i < *nai; i++ ) {
		if (iai[i] < 0) itm=0;
	}
	if (itm != 0) {
		it1=0;
		for( i = 0; i < *nai; i++ ) {
            if (iag[i] == 1) it1=iai[i];
			if (iag[i] == 2) it2=iai[i];
		}
		if (it1 > 0 && it2 != 1) {
/*
 * Set fill area color index.
 */
            gset_fill_colr_ind(it1);
/*
 * Create structure to pass to gfill_area
 */
			fill_area.num_points = *ncs-1;
			fill_area.points = (Gpoint *) malloc(fill_area.num_points*sizeof(Gpoint));
			if( !fill_area.points ) {
				fprintf( stderr, "colram: Not enough memory to create fill area structure\n" );
				gemergency_close_gks();
				exit(1);
			}
			for( i = 0; i < *ncs-1; i++ ) {
				fill_area.points[i].x = xcs[i];
				fill_area.points[i].y = ycs[i];
			}
/*
 * Fill area
 */
            gfill_area (&fill_area);
			free(fill_area.points);
		}
	}
	return(0);
}

int colrln(
    float *xcs,
    float *ycs,
    int *ncs,
    int *iai,
    int *iag,
    int *nai
)
{
	int i, itm, it1, it2;
	Gpoint_list line;

	itm=1;
	for( i = 0; i < *nai; i++ ) {
		if (iai[i] < 0) itm=0;
	}
	if (itm != 0) {
		it1=0;
		it2=0;
		for( i = 0; i < *nai; i++ ) {
			if (iag[i] == 1) it1=iai[i];
			if (iag[i] == 2) it2=iai[i];
		}
		if (it1 > 0 && it2 != 1) {
/*
 * Flush PLOTIT's buffers and set polyline color index.
 */     
            c_plotit(0,0,0);
            gset_line_colr_ind(mod(it1+3,15)+1);
/*
 * Create structure to pass to gpolyline
 */
			line.num_points = *ncs;
			line.points = (Gpoint *) malloc(line.num_points*sizeof(Gpoint));
			if( !line.points ) {
				fprintf( stderr, "colrln: Not enough memory to create line structure\n" );
				gemergency_close_gks();
				exit(1);
			}
			for( i = 0; i < *ncs; i++ ) {
				line.points[i].x = xcs[i];
				line.points[i].y = ycs[i];
			}
            gpolyline(&line);
		}
	}
	return(0);
}


