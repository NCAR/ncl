/*
 *	$Id: c_cpex10.c,v 1.1 1994-10-31 02:26:27 haley Exp $
 */

#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

/*
 * Declare the common block in which the angle at which the label of a
 * point on the globe is to be written and the latitude and longitude
 * of the point being labelled are transmitted to the routine PCMPXY, in
 * the package PLOTCHAR.
 */
#if !defined (cray)
extern struct common {
#else
struct common {
#endif
	float pang,plat,plon;
} NGCALLF(pcmp04,PCMP04);

/*
 * Define arrays to hold a list of state names, two-character mnemonics
 * for the states, and the latitude and longitude of a point where the
 * mnemonic may be placed to label the state.
 */

/*
 * Define the state-labelling data.
 */
char *snam[50] = {
		"Alabama       " , "Alaska        " , "Arizona       " , 
		"Arkansas      " , "California    " , "Colorado      " , 
		"Connecticut   " , "Delaware      " , "Florida       " , 
		"Georgia       " , "Hawaii        " , "Idaho         " , 
		"Illinois      " , "Indiana       " , "Iowa          " , 
		"Kansas        " , "Kentucky      " , "Louisiana     " , 
		"Maine         " , "Maryland      " , "Massachusetts " , 
		"Michigan      " , "Minnesota     " , "Mississippi   " , 
		"Missouri      " , "Montana       " , "Nebraska      " , 
		"Nevada        " , "New Hampshire " , "New Jersey    " , 
		"New Mexico    " , "New York      " , "North Carolina" , 
		"North Dakota  " , "Ohio          " , "Oklahoma      " , 
		"Oregon        " , "Pennsylvania  " , "Rhode Island  " , 
		"South Carolina" , "South Dakota  " , "Tennessee     " , 
		"Texas         " , "Utah          " , "Vermont       " ,
		"Virginia      " , "Washington    " , "West Virginia " ,
		"Wisconsin     " , "Wyoming       " };

char *smne[50] = {
	"AL" , "AK" , "AZ" , "AR" , "CA" , "CO" , "CT" , "DE" , "FL" , "GA" , 
	"HI" , "ID" , "IL" , "IN" , "IA" , "KS" , "KY" , "LA" , "ME" , "MD" , 
	"MA" , "MI" , "MN" , "MS" , "MO" , "MT" , "NE" , "NV" , "NH" , "NJ" , 
	"NM" , "NY" , "NC" , "ND" , "OH" , "OK" , "OR" , "PA" , "RI" , "SC" ,
	"SD" , "TN" , "TX" , "UT" , "VT" , "VA" , "WA" , "WV" , "WI" , "WY" };

float slat[50] = {
	33.0 , 65.0 , 34.7 , 35.0 , 37.5 , 39.0 , 41.6 , 39.0 , 28.5 , 32.5 , 
	20.0 , 43.5 , 40.2 , 40.0 , 42.0 , 38.5 , 37.4 , 31.2 , 45.5 , 39.2 , 
	42.3 , 44.0 , 46.0 , 32.5 , 38.5 , 47.0 , 41.5 , 39.8 , 43.2 , 39.7 , 
	34.7 , 43.0 , 35.5 , 47.5 , 40.2 , 35.6 , 44.0 , 40.8 , 41.7 , 34.0 , 
	44.5 , 36.0 , 32.0 , 39.5 , 44.2 , 37.6 , 47.5 , 38.5 , 44.5 , 43.0 };

float slon[50] = {
	-86.5 , -152.0 , -111.5 ,  -92.5 , -120.5 , -105.8 ,  -72.6 ,  -75.5 ,
	-82.0 ,  -83.0 , -157.0 , -114.0 ,  -89.2 ,  -86.0 ,  -93.2 ,  -98.2 ,
	-84.5 ,  -92.5 ,  -69.0 ,  -76.5 ,  -72.0 ,  -85.0 ,  -94.5 ,  -89.5 ,
	-92.5 , -109.5 ,  -99.5 , -117.0 ,  -71.6 ,  -74.5 , -106.0 ,  -75.0 ,
	-79.5 , -100.5 ,  -82.5 ,  -97.5 , -120.2 ,  -77.6 ,  -71.5 ,  -80.5 ,
	-100.5 ,  -86.5 , -100.0 , -111.5 ,  -72.5 ,  -78.6 , -120.5 ,  -80.8 ,
	-89.5 , -107.5};
/*
 * This program shows a view of the area around Boulder, Colorado, as
 * seen from a satellite directly above Washington, D.C.  Within a
 * circular area near Boulder, color-filled contour bands are shown;
 * state boundaries are drawn in black over them.  Outside that area,
 * the surface of the globe is shaded gray, state boundaries are drawn
 * in white, and lines of latitude and longitude are drawn in a light
 * gray.  Two-letter mnemonics are used to identify the states; each
 * is written using new mapping capabilities of the package PLOTCHAR.
 *
 * Define parameters specifying the lengths of the two area maps and
 * the real and integer workspaces that are used.
 */

#define LAM1 2000
#define LAM2 30000
#define LRWK 200
#define LIWK 100

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
/*
 * Declare an array in which to put an area map that will be used to
 * distinguish the circular area near Boulder from the rest of the globe
 * and another array in which to put an area map that will be used to
 * color contour bands within that area.  This program could be written
 * using a single area map; it is being done this way for illustrative
 * purposes.
 */
	int iam1[LAM1],iam2[LAM2];
/*
 * Declare scratch arrays required by the routine ARSCAM, MAPBLM, and
 * MAPGRM.
 */
	float xcra[1000],ycra[1000];
	int iaia[10],iagi[10];
/*
 * Declare arrays in which to get the latitudes and longitudes of points
 * defining a circle around Boulder on the surface of the globe.
 */
	float clat[100],clon[100];
/*
 * Declare arrays in which to get the latitudes and longitudes of points
 * defining a star over Boulder.
 */
	float xlat[6],xlon[6];
/*
 * Define an array in which to generate some dummy data to contour and
 * workspace arrays required by CONPACK.
 */
	float zdat[41][41],rwrk[LRWK];
	int iwrk[LIWK];
/*
 * Declare external the routines that will draw the masked grid lines
 * and geographical outlines and the routine that will color the contour
 * bands in the circular area around Boulder.
 */
	extern drawgl(),drawgo(),colrcb();
/*
 * Define multiplicative constants to convert from degrees to radians
 * and from radians to degrees.
 */
	float dtor = .017453292519943;
	float rtod = 57.2957795130823;

	int i, j, irws, iiws;
	float x, y, dfce, pang;
    float plm1[2],plm2[2],plm3[2],plm4[2];

	Gcolr_rep rgb;
/*
 * Get the latitudes and longitudes of 100 points defining a circle on
 * the globe centered at the point (40.0,-105.3) - the approximate
 * latitude and longitude of Boulder, Colorado - and having a radius
 * of 7 degrees.
 */
	c_nggcog (40.0,-105.3,7.0,clat,clon,100);
/*
 * Generate some dummy data to contour later.
 */
	for( i = 0; i < 41; i++ ) {
		x = (float)i/40.;
		for( j = 0; j < 41; j++ ) {
            y = (float)j/40.;
            zdat[j][i]=x*x+y*y+x*y+sin(9.*x)*cos(9.*y);
		}
	}
/*
 * Open GKS.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Turn off clipping.
 */
	gset_clip_ind (GIND_NO_CLIP);
/*
 * Turn on solid fill.
 */
	gset_fill_int_style (GSTYLE_SOLID);
/*
 * Define some colors to use.  Color index 2 is for grid lines far from
 * Boulder, color index 3 is for geographical objects near Boulder, and
 * color index 4 is for geographical objects far from Boulder.  Color
 * index 5 is used for that part of the earth which is not within the
 * circle around Boulder.  Color index 6 is used for the star over
 * Boulder.  Color indices 101 through 116 are to be used for contour
 * bands in the area near Boulder; they are evenly distributed between
 * pure red and pure blue.
 */
	rgb.rgb.red = .6; rgb.rgb.green = .6; rgb.rgb.blue = .6;
	gset_colr_rep (WKID,2,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep (WKID,3,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep (WKID,4,&rgb);
	rgb.rgb.red = .4; rgb.rgb.green = .4; rgb.rgb.blue = .4;
	gset_colr_rep (WKID,5,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
	gset_colr_rep (WKID,6,&rgb);

	for( i = 101; i <= 116; i++ ) {
		rgb.rgb.red = (float)(float)(116-i)/15.;
		rgb.rgb.green = 0.;
		rgb.rgb.blue = (float)(i-101)/15.;
		gset_colr_rep(WKID,i,&rgb);
	}
/*
 * Put a label at the top of the plot.
 */
	c_set    (0.,1.,0.,1.,0.,1.,0.,1.,1);
	c_plchhq (.5,.975,
              ":F25:SATELLITE VIEW OF CONTOUR BANDS IN A LIMITED AREA",
              .018,0.,0.);
/*
 * Tell EZMAP where to put the map on the plotter frame.
 */
	c_mappos (.05,.95,.05,.95);
/*
 * Tell EZMAP to use the view from a satellite above Washington, D.C.
 * The basic satellite-view projection is rotated clockwise by 75
 * degrees so that the direction a little north of due west is toward
 * the top of the projection.
 */
	c_maproj ("SV",38.,-76.,75.);
/*
 * Tell EZMAP how far the satellite is from the center of earth and
 * make it look in a direction about 7/8 of the way between looking
 * straight down and looking directly at the horizon.  We end up
 * looking roughly in the direction of Boulder.
 */
	dfce=1.3;

	c_mpsetr ("SA",dfce);

	c_mpsetr ("S1",7.*rtod*asin(1./dfce)/8.);
/*
 * Set the parameter "S2" so that the line of sight is displaced toward
 * the top of the basic satellite view - that is to say, in the direction
 * a little north of due west that the setting of ROTA implies - by the
 * angle specified by "S1".
 */
	c_mpsetr ("S2",90.);
/*
 * Tell EZMAP the satellite has a total field of view of 40 degrees.
 */
    plm1[0] = plm2[0] = plm3[0] = plm4[0] = 20;
    plm1[1] = plm2[1] = plm3[1] = plm4[1] = 0;
	c_mapset ("AN",plm1,plm2,plm3,plm4);
/*
 * Tell EZMAP to use the outline with political and state boundaries.
 */
	c_mpsetc ("OU","PS");
/*
 * Tell EZMAP to use a one-degree grid.
 */
	c_mpseti ("GR",1);
/*
 * Initialize EZMAP.
 */
	c_mapint();
/*
 * Tell CONPACK not to call SET (because MAPINT has already done it).
 */
	c_cpseti ("SET - DO-SET-CALL FLAG",0);
/*
 * Tell CONPACK to map the contour lines using EZMAP.
 */
	c_cpseti ("MAP - MAPPING FLAG.",1);
/*
 * Tell CONPACK what longitudes the minimum and maximum values of the
 * first array index correspond to.
 */
	c_cpsetr ("XC1 - X COORDINATE AT I=1",-115.);
	c_cpsetr ("XCM - X COORDINATE AT I=M", -95.);
/*
 * Tell CONPACK what latitudes the minimum and maximum values of the
 * second array index correspond to.
 */
	c_cpsetr ("YC1 - Y COORDINATE AT J=1",32.);
	c_cpsetr ("YCN - Y COORDINATE AT J=N",48.);
/*
 * Tell CONPACK to use exactly 15 contour levels, splitting the range
 * from the minimum value to the maximum value into 16 equal bands.
 */
	c_cpseti ("CLS - CONTOUR LEVEL SELECTOR",-15);
/*
 * Tell CONPACK where the data to be contoured are, where the real and
 * integer workspaces are, and how big each array is.
 */
	c_cprect (&zdat[0][0],41,41,41,rwrk,LRWK,iwrk,LIWK);
/*
 * Initialize the first area-map array.
 */
	c_arinam (iam1,LAM1);
/*
 * Put the projection of the circle around Boulder into the area map.
 * The information goes into edge group 1.  The area inside the projected
 * circle is characterized as area 1 and the area outside the circle as
 * area 2.
 */
	c_mapita (clat[0],clon[0],0,iam1,1,1,2);

	for( i = 1; i < 100; i++ ) {
		c_mapita (clat[i],clon[i],1,iam1,1,1,2);
	}

	c_mapiqa (iam1,1,1,2);
/*
 * Copy the information from the first area-map array to the second one.
 * Note that the routine we use to do this, despite the "move" implied
 * by its name, can actually be used in this way.
 */
	c_armvam (iam1,iam2,LAM2);
/*
 * Add to the second area map the limb line and the perimeter for the
 * satellite-view projection.  This is done by temporarily using no
 * outline dataset, so that MAPBLA will put only the lines we want into
 * the area map.  The edges will go in edge group 1.
 */
	c_mpsetc ("OU","NO");
	c_mapbla (iam2);
	c_mpsetc ("OU","PS");
/*
 * Add to the second area map the contour lines for the area near
 * Boulder.  They will go in edge group 3.
 */
	c_cpclam (&zdat[0][0],rwrk,iwrk,iam2);
/*
 * Scan the second area map to color the contour bands near Boulder.
 */
	c_arscam (iam2,xcra,ycra,1000,&iaia[0],&iagi[0],10,colrcb);
/*
 * Double the line width.
 */
	gset_linewidth (2.);
/*
 * Draw a masked latitude/longitude grid and masked outlines.
 */
	c_mapgrm (iam1,xcra,ycra,1000,iaia,iagi,10,drawgl);
	c_mapblm (iam1,xcra,ycra,1000,&iaia[0],&iagi[0],10,drawgo);
/*
 * Set the current polyline color index to draw a black line.
 */
	gset_line_colr_ind (0);
/*
 * Draw the circle around Boulder.
 */
	c_mapit (clat[0],clon[0],0);

	for( i = 1; i < 100; i++ ) {
		c_mapit (clat[i],clon[i],1);
	}

	c_mapiq();
/*
 * Set the current polyline color index to draw a yellow line.
 */
	gset_line_colr_ind (6);
/*
 * Put a star at the position of Boulder.
 */
	c_nggsog (40.,-105.,.25,xlat,xlon);

	c_mapit (xlat[0],xlon[0],0);

	for( i = 1; i < 6; i++ ) {
		c_mapit (xlat[i],xlon[i],1);
	}

	c_mapiq();
/*
 * Label the states using two-character mnemonics for them.
 */
	c_pcseti ("MAP",4);
	c_pcsetr ("ORV",1.e12);
	NGCALLF(pcmp04,PCMP04).pang = 45.;

	for( i = 0; i < 50; i++ ) {
		NGCALLF(pcmp04,PCMP04).plat = slat[i];
		NGCALLF(pcmp04,PCMP04).plon = slon[i];
		c_plchhq (0.,0.,smne[i],.5,0.,0.);
	}
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
/*
 * Compute or get information about the amount of space used in each
 * area-map array and in the real and integer workspace arrays and
 * print it.
 */
	c_cpgeti ("RWU - INTEGER WORKSPACE USED",&irws);
	c_cpgeti ("IWU - INTEGER WORKSPACE USED",&iiws);

	printf( "NUMBER OF WORDS USED IN REAL WORKSPACE: %d\n",irws );
	printf( "NUMBER OF WORDS USED IN INTEGER WORKSPACE: %d\n",iiws );
	printf( "NUMBER OF WORDS USED IN AREA-MAP ARRAY 1: %d\n", LAM1-(iam1[5]-iam1[4]-1));
	printf("NUMBER OF WORDS USED IN AREA-MAP ARRAY 2: %d \n",LAM2-(iam2[5]-iam2[4]-1));
/*
 * Done.
 */
}

int drawgl(xcra,ycra,ncra,iaai,iagi,nogi)
float *xcra, *ycra;
int *iaai, *iagi, *ncra, *nogi;
{
/*
 * The routine DRAWGL draws the polyline defined by the points
 * ((XCRA(I),YCRA(I)),I=1,NCRA) using color index 2 if the area
 * identifier relative to edge group 1 is a 2, implying that the
 * polyline is outside the circle around Boulder, Colorado; otherwise,
 * it does not draw the polyline at all.
 *
 * Find the area identifier relative to edge group 1.
 */
	int i, iai1;

	iai1 = -1;

	for( i = 0; i < *nogi; i++ ) {
		if (iagi[i] == 1) iai1 = iaai[i];
	}
/*
 * If the polyline is outside the circle, draw it.
 */
	if (iai1 == 2) {
		gset_line_colr_ind (2);
		c_curve (xcra,ycra,*ncra);
	}
/*
 * Done.
 */
	return(0);
}

int drawgo(xcra,ycra,ncra,iaai,iagi,nogi)
float *xcra, *ycra;
int *iaai, *iagi, *ncra, *nogi;
{
/*
 * The routine DRAWGO draws the polyline defined by the points
 * ((XCRA(I),YCRA(I)),I=1,NCRA) using color index 3 if the area
 * identifier relative to edge group 1 is a 1, implying that the
 * polyline is inside the circle around Boulder, Colorado, and
 * using color index 4, otherwise.
 *
 * Find the area identifier relative to edge group 1.
*/
	int i, iai1;

	iai1 = -1;

	for( i = 0; i < *nogi; i++ ) {
		if (iagi[i] == 1) iai1 = iaai[i];
	}
/* 
 * Draw the polyline if the area identifier is a 1 or a 2, but not
 * otherwise.
 */
	if (iai1 == 1) {
		gset_line_colr_ind (3);
	}
	else if (iai1 == 2) {
		gset_line_colr_ind (4);
	}

	c_curve (xcra,ycra,*ncra);
/*
 * Done.
 */
	return(0);

}

int colrcb(xcra,ycra,ncra,iaai,iagi,nogi)
float *xcra, *ycra;
int *iaai, *iagi, *ncra, *nogi;
{
/*
 * The routine COLRCB colors the polygon defined by the points
 * ((XCRA(I),YCRA(I)),I=1,NCRA) if and only if it is inside the
 * circle around Boulder and it is a portion of one of the contour
 * bands defined by the dummy data array.
 *
 * Find the area identifiers for the polygon relative to edge groups 1
 * and 3.
 */
	Gpoint_list fill_area;
	int i, iai1, iai3;

	iai1 = -1;
	iai3 = -1;

	for( i = 0; i < *nogi; i++ ) {
		if (iagi[i] == 1) iai1 = iaai[i];
		if (iagi[i] == 3) iai3 = iaai[i];
	}
/*
 * Fill the polygon using a color implied by the contour level if it
 * is inside the circle around Boulder and is part of a contour band.
 * If it is outsie the circle around Boulder, but is still on the
 * globe, use color index 5 for it.
 */
/*
 * Create structure to pass to gfill_area
 */
	fill_area.num_points = *ncra-1;
	fill_area.points = (Gpoint *) malloc(fill_area.num_points*sizeof(Gpoint));
	if( !fill_area.points ) {
		fprintf( stderr, "colrcb: Not enough memory to create fill area structure\n" );
		gemergency_close_gks();
		exit(1);
	}
	for( i = 0; i < *ncra-1; i++ ) {
		fill_area.points[i].x = xcra[i];
		fill_area.points[i].y = ycra[i];
	}
	if (iai1 == 1 && iai3 >= 1 && iai3 <= 16) {
		gset_fill_colr_ind (100+iai3);
		gfill_area(&fill_area);
	}
	else if (iai1 == 2) {
		gset_fill_colr_ind (5);
		gfill_area(&fill_area);
	}
/*
 * Done.
 */
    free(fill_area.points);
	return(0);
}
