/*
 *	$id: c_arex01.c.sed,v 1.3 1994/06/21 14:53:52 haley exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

/*
 * Define the size of the area map array to be used.  The part of this
 * used by the areas routines will fluctuate as needed.
 */
#define LAMA 6000

/*
 * Define the sizes of the work arrays to be used by arscam.
 */
#define NCRA 1000
#define NGPS 10

#define WSTYPE SED_WSTYPE
#define WKID   1

#define min(x,y)   ((x) < (y) ? (x) : (y))

/*
 * Define the perimeter for all three groups of edges.
 */
float perix[5] = {.90,.90,.10,.10,.90};
float periy[5] = {.10,.90,.90,.10,.10};

/*
 * Define the group 1 edges.
 */
float g1e1x[9] = {.75,.68,.50,.32,.25,.32,.50,.68,.75};
float g1e1y[9] = {.50,.68,.75,.68,.50,.32,.25,.32,.50};
float g1e2x[5] = {.60,.60,.40,.40,.60};
float g1e2y[5] = {.40,.60,.60,.40,.40};

/*
 * Define the group 5 edges.
 */
float g5e1x[7] = {.50,.80,.80,.50,.50,.20,.35};
float g5e1y[7] = {.50,.50,.80,.80,.50,.20,.35};

main()
{
	Gcolr_rep rgb;
/*
 * this program illustrates the use of the routine armvam, particularly
 * as it is used in the process of recovering from area map overflow
 * problems.  Because this example is intended to be strictly fortran
 * 77, no attempt is made to do real dynamic storage allocation;
 * instead, an area map array of a fixed size is used, and the areas
 * routines are only told about a part of that array.  Still, the idea
 * is much the same as one would use in c or, presumably, in fortran
 * 90 (if and when compilers become generally available for fortran 90).
 *
 * Declare the area map array.
 */
	int i, iama[LAMA], nama;
/*
 * Declare the work arrays to be used by arscam.
 */
	float xcra[NCRA],ycra[NCRA],iaai[NGPS],iagi[NGPS], s;
/*
 * Declare arrays for group 3 edges.
 */
	float g3e1x[2],g3e1y[2];
	float g3e2x[2],g3e2y[2];
	float g3e3x[2],g3e3y[2];
	float g3e4x[2],g3e4y[2];
	float g3e5x[2],g3e5y[2];
	float g3e6x[2],g3e6y[2];
	float g3e7x[2],g3e7y[2];
	float g3e8x[2],g3e8y[2];
	float g3e9x[2],g3e9y[2];
/*
 * Declare the routine that will color the areas defined by the area map.
 */
	extern int colram();
	extern void exedam(), expram(), exscam();
/*
 * Define the group 3 edges.
 */
	g3e1x[0] =  .10; g3e1x[1] = .20;
	g3e1y[0] =  .80; g3e1y[1] = .90;

	g3e2x[0] =  .10; g3e2x[1] = .40;
	g3e2y[0] =  .60; g3e2y[1] = .90;

	g3e3x[0] =  .10; g3e3x[1] = .60;
	g3e3y[0] =  .40; g3e3y[1] = .90;

	g3e4x[0] =  .10; g3e4x[1] = .80;
	g3e4y[0] =  .20; g3e4y[1] = .90;

	g3e5x[0] =  .20; g3e5x[1] = .90;
	g3e5y[0] =  .10; g3e5y[1] = .80;

	g3e6x[0] =  .40; g3e6x[1] = .90;
	g3e6y[0] =  .10; g3e6y[1] = .60;

	g3e7x[0] =  .60; g3e7x[1] = .90;
	g3e7y[0] =  .10; g3e7y[1] = .40;

	g3e8x[0] =  .80; g3e8x[1] = .90;
	g3e8y[0] =  .10; g3e8y[1] = .20;

	g3e9x[0] =  .40; g3e9x[1] = .20;
	g3e9y[0] =  .70; g3e9y[1] = .50;
/*
 * Open gks.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Change the gks "fill area interior style" to be "solid".
 */
	gset_fill_int_style(GSTYLE_SOLID);
/*
 * Define the colors to be used.
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep (WKID,0,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep (WKID,1,&rgb);

	for( i = 1; i <= 9; i++ ) {
		s=(float)i/9.;
		rgb.rgb.red = s; rgb.rgb.green = 1.-s; rgb.rgb.blue = 0.;
		gset_colr_rep (WKID,10+i,&rgb);
		rgb.rgb.red = s; rgb.rgb.green = 1.-s; rgb.rgb.blue = 1.;
		gset_colr_rep (WKID,20+i,&rgb);
	}
/*
 * Define the mapping from the user system to the plotter frame.
 */
	c_set (.05,.95,.05,.95,0.,1.,0.,1.,1);
/*
 * Initialize the variable that keeps track of how much space is
 * currently used in the area map.
 */
	nama=500;
/*
 * Initialize the area map.
 */
	c_arinam (iama,nama);
/*
 * Put group 1 edges into the area map.  Instead of calling the areas
 * routine aredam directly, we call an example routine that puts the
 * package seter into recovery mode, checks for overflow of the area
 * map array, and uses armvam to recover.
 */
	exedam (iama,LAMA,&nama,perix,periy,5,1, 0,-1);
	exedam (iama,LAMA,&nama,g1e1x,g1e1y,9,1, 2, 1);
	exedam (iama,LAMA,&nama,g1e2x,g1e2y,5,1, 1, 2);
/*
 * Put group 3 edges into the area map.  Again, instead of calling the
 * areas routine aredam directly, we call exedam to allow for error
 * recovery.
 */
	exedam (iama,LAMA,&nama,perix,periy,5,3, 0,-1);
	exedam (iama,LAMA,&nama,g3e1x,g3e1y,2,3, 1, 2);
	exedam (iama,LAMA,&nama,g3e2x,g3e2y,2,3, 2, 3);
	exedam (iama,LAMA,&nama,g3e3x,g3e3y,2,3, 3, 4);
	exedam (iama,LAMA,&nama,g3e4x,g3e4y,2,3, 4, 5);
	exedam (iama,LAMA,&nama,g3e5x,g3e5y,2,3, 5, 6);
	exedam (iama,LAMA,&nama,g3e6x,g3e6y,2,3, 6, 7);
	exedam (iama,LAMA,&nama,g3e7x,g3e7y,2,3, 7, 8);
	exedam (iama,LAMA,&nama,g3e8x,g3e8y,2,3, 8, 9);
	exedam (iama,LAMA,&nama,g3e9x,g3e9y,2,3, 9,10);
/*
 * Put group 5 edges into the area map.  Again, instead of calling the
 * areas routine aredam directly, we call exedam to allow for error
 * recovery.
 */
	exedam (iama,LAMA,&nama,perix,periy,5,5, 0,-1);
	exedam (iama,LAMA,&nama,g5e1x,g5e1y,7,5,-1, 0);
/*
 * Preprocess the area map.  Again, instead of calling the areas
 * routine arpram directly, we call expram to allow for error
 * recovery.  Do debug plots to make sure things are working okay.
 */
	c_ardbpa (iama,1,"BEFORE CALLING ARPRAM - GROUP 1");
	c_ardbpa (iama,3,"BEFORE CALLING ARPRAM - GROUP 3");
	c_ardbpa (iama,5,"BEFORE CALLING ARPRAM - GROUP 5");
	expram (iama,LAMA,&nama,0,0,0);
	c_ardbpa (iama,1,"AFTER CALLING ARPRAM - GROUP 1");
	c_ardbpa (iama,3,"AFTER CALLING ARPRAM - GROUP 3");
	c_ardbpa (iama,5,"AFTER CALLING ARPRAM - GROUP 5");
/*
 * Pack the contents of the area map into the smallest possible space.
 */
	c_armvam (iama,iama,iama[0]-(iama[5]-iama[4]-1));
/*
 * Scan the area map.  Again, instead of calling the areas routine
 * arscam directly, we call exscam to allow for error recovery.
 */
	exscam (iama,LAMA,&nama,xcra,ycra,NCRA,iaai,iagi,NGPS);
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Close gks.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

/*
 * This routine colors the areas defined by the area map.
 */
int colram (xcra,ycra,ncra,iaai,iagi,ngps)
float *xcra, *ycra;
int *ncra, *iaai, *iagi, *ngps;
{
	Gpoint_list fill_area;
/*
 * Pick off the individual group identifiers.
 */
	int i, iai1, iai3, iai5;

	iai1=0;
	iai3=0;
	iai5=0;

	for( i = 0; i < *ngps; i++ ) {
		if (iagi[i] == 1) iai1=iaai[i];
		if (iagi[i] == 3) iai3=iaai[i];
		if (iagi[i] == 5) iai5=iaai[i];
	}
/*
 * Skip coloring if either of the first two area identifiers is zero or
 * negative or if the final one is negative.
 */
	if (iai1 <= 0 || iai3 <= 0 || iai5 < 0) return(0);
/*
 * Otherwise, color the area, using a color index which is obtained by
 * combining the area identifiers for groups 1 and 3.
 */
	gset_fill_colr_ind (10*iai1+iai3);
	fill_area.num_points = *ncra-1;
	fill_area.points = (Gpoint *) malloc(fill_area.num_points*sizeof(Gpoint));
	if( !fill_area.points ) {
		fprintf( stderr, "colram: Not enough memory to create fill area structure\n" );
		gemergency_close_gks();
		exit(1);
	}
	for( i = 0; i < *ncra-1; i++ ) {
		fill_area.points[i].x = xcra[i];
		fill_area.points[i].y = ycra[i];
	}
	gfill_area (&fill_area);
	free(fill_area.points);
/*
 * Done.
 */
	return(0);
}

void exedam (iama,lama,nama,xcra,ycra,ncra,igid,iail,iair)
int *iama, lama, *nama, ncra, igid, iail, iair;
float *xcra, *ycra;
{
	int irold, nerr;
	char ermsg[114];
/*
 * Put seter into recovery mode, saving the previous setting of the
 * recovery-mode flag in irold.
 */
	c_entsr (&irold,1);
/*
 * Attempt to put the edges in the error map.
 */
one:
	c_aredam (iama,xcra,ycra,ncra,igid,iail,iair);
/*
 * See if a recoverable error occurred during the call to aredam.
 */
	if (c_nerro(&nerr) != 0) {
/*
 * A recoverable error occurred.  See if it was due to overflowing the
 * area map array and if we can do something about it.
 */
		printf( "SEMESS(2)\n%s\n", c_semess(2) );
		printf( "NAMA %d\n", *nama );
		printf( "LAMA %d\n", lama );
		strcpy(ermsg, c_semess(2));
		if (!strcmp(ermsg,"AREA-MAP ARRAY OVERFLOW") &&  *nama < LAMA) {
/*
 * Recover from an area map array overflow.  first, log what's happening.
 */
            printf ("EXEDAM - OVERFLOW RECOVERY - NAMA = %d\n", *nama );
/*
 * Clear the internal error flag in seter.
 */
            c_errof();
/*
 * Move the area map to a slightly larger part of the area map array.
 */
            *nama=min(*nama+100,lama);
            c_armvam (iama,iama,*nama);
/*
 * Go back to try the call to aredam again.
 */
            goto one;
		}
/*
 * Either the error is not an overflow error or we can't do anything
 * about it.  Exit with a fatal error message.
 */
		c_seter ("EXEDAM - CAN'T GET AROUND AREDAM ERROR",1,2);
	}
	else {
/*
 * No recoverable error occurred.  Restore the original value of seter"s
 * recovery-mode flag.
 */
		c_retsr (irold);
	}
/*
 * Done.
 */
	return;
}

void expram (iama,lama,nama,ifl1,ifl2,ifl3)
int *iama, lama, *nama, ifl1, ifl2, ifl3;
{
	int irold, nerr;
	char ermsg[114];
/*
 * Put seter into recovery mode, saving the previous setting of the
 * recovery-mode flag in irold.
 */
	c_entsr (&irold,1);
/*
 * Attempt to pre-process the area map.
 */
two: 
	c_arpram (iama,ifl1,ifl2,ifl3);
/*
 * See if a recoverable error occurred during the call to arpram.
 */
	if (c_nerro(&nerr) != 0) {
/*
 * A recoverable error occurred.  See if it was due to overflowing the
 * area map array and if we can do something about it.
 */
		strcpy(ermsg, c_semess(2));
		if (!strcmp(ermsg,"AREA-MAP ARRAY OVERFLOW") && *nama < LAMA) {
/*
 * Recover from an area map array overflow.  first, log what's happening.
 */
            printf( "EXPRAM - OVERFLOW RECOVERY - NAMA = %d\n", *nama );
/*
 * Clear the internal error flag in seter.
 */
            c_errof();
/*
 * Move the area map to a slightly larger part of the area map array.
 */
            *nama=min(*nama+100,lama);
            c_armvam (iama,iama,*nama);
/*
 * Go back to try the call to arpram again.
 */
            goto two;
		}
        else {
/*
 * Either the error is not an overflow error or we can't do anything
 * about it.  Exit with a fatal error message.
 */
            c_seter ("EXPRAM - CAN'T GET AROUND ARPRAM ERROR",1,2);
        }
    }
    else {
/*
 * No recoverable error occurred.  Restore the original value of seter's
 * recovery-mode flag.
 */
        c_retsr (irold);
    }
/*
 * Done.
 */
    return;
}

void exscam (iama,lama,nama,xcra,ycra,ncra,iaai,iagi,ngps)
int *iama,*iaai,*iagi, lama, *nama, ngps;
float *xcra,*ycra;
{
    int irold, nerr;
	char ermsg[114];
	extern int colram();
/*
 * Put seter into recovery mode, saving the previous setting of the
 * recovery-mode flag in irold.
 */
    c_entsr (&irold,1);
/*
 * Attempt to scan the area map.
 */
three:
    c_arscam (iama,xcra,ycra,ncra,iaai,iagi,ngps,colram);
/*
 * See if a recoverable error occurred during the call to arscam.
 */
    if (c_nerro(&nerr) != 0) {
/*
 * A recoverable error occurred.  See if it was due to overflowing the
 * area map array and if we can do something about it.
 */
		strcpy(ermsg, c_semess(2));
		if (!strcmp(ermsg,"AREA-MAP ARRAY OVERFLOW") && *nama < LAMA) {
/*
 * Recover from an area map array overflow.  first, log what's happening.
 */
            printf( "EXSCAM - OVERFLOW RECOVERY - NAMA = %d\n", *nama );
/*
 * Clear the internal error flag in seter.
 */
            c_errof();
/*
 * Move the area map to a slightly larger part of the area map array.
 */
            *nama=min(*nama+100,lama);
            c_armvam (iama,iama,*nama);
/*
 * Go back to try the call to arpram again.
 */
            goto three;
		}
		else {
/*
 * Either the error is not an overflow error or we can't do anything
 * about it.  Exit with a fatal error message.
 */
            c_seter ("EXSCAM - CAN'T GET AROUND ARSCAM ERROR",1,2);
		}
	}
	else {
/*
 * No recoverable error occurred.  Restore the original value of seter's
 * recovery-mode flag.
 */
		c_retsr (irold);
	}

/*
 * Done.
 */
	return;
}

