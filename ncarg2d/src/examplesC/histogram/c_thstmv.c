/*
 *	$Id: c_thstmv.c,v 1.2 2003-03-03 16:16:24 haley Exp $
 */
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define  NDIM   320
#define  NCLS   17
#define  NWRK   374

#define IWTYPE 1
#define WKID   1

main()
{
/*
 * PURPOSE                To provide a demonstration of the Histogram
 *                        utility when special flagged values occur in
 *                        the input data.
 *
 * USAGE                  CALL THSTMV (IERROR)
 *
 * ARGUMENTS
 *
 *
 * I/O                    If the test is successful, the message
 *
 *                          HISTGR TEST SUCCESSFUL  . . .  SEE PLOT
 *                          TO VERIFY PERFORMANCE
 *
 *                        is written on unit 6.
 *
 *                        Verify success by examing the plot produced.
 *
 * PRECISION              Single
 *
 * REQUIRED LIBRARY       HISTGR
 * FILES
 *
 * REQUIRED GKS LEVEL     0A
 *
 * LANGUAGE               FORTRAN 77
 *
 * HISTORY                Written by Bob Lackman, October 1993.
 *
 * ALGORITHM              THSTMV computes data and calls HISTGR
 *                        with special values
 *
 *
 *  Array DAT1 is filled with values to be used as input for HISTGR
 */
/*
 *  NWRK = NDIM + 3*(NCLS+1)
 */
	float dat1[2][NDIM], x, work[NWRK];
	float  class[NCLS+1];
	float array[2];
	int i, j, l, iflag, nclass, npts;
	char ifc[2];

/*
 * Open GKS.
 */
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, IWTYPE);
    gactivate_ws(WKID);
/*
 * Change the Plotchar special character code from a : to a @
 */
	strcpy( ifc, "@" );
	c_pcsetc("FC",ifc);
/*
 * Change the print font to the high quality filled times roman.
 */
	c_pcsetc("FN","times-roman");
/*
 *     Plot 1:   IFLAG = 0, input values are sorted into 11 equal classes
 */
	iflag = 0;
	nclass = 11;
	npts = 320;

	for( i = 0; i < npts; i++ ) {
		for( j = 0; j < 2; j++ ) {
			dat1[j][i] = 0.;
		}
	}
	
	for( i = 0; i < npts; i++ ) {
        x = (float)i;
        dat1[0][i] = 10. * log10(0.1*x+1.);
	}
/*
 * Put in some special values
 */
	dat1[0][6]  = -999.;
	dat1[0][43] = -999.;
	dat1[0][78] = -999.;

	for( l = 199; l < 320; l++ ) {
		dat1[0][l] = -999.;
	}
/*
 *  (First call HSTOPL("DEF=ON") to activate all default options.)
 */
	c_hstopl("DE=ON");
/*
 *  Then call HSTOPR to activate special value checking
 */
	array[0] = -999.;
	array[1] = 1.e-6;
	c_hstopr("MV=ON",array,2);
/*
 *  Call HSTOPL with NMV = ON, so normalization is by NPTS-MISS.
 */
	c_hstopl("NM=ON");
/*
 *  Call HSTOPL with PMV = ON, so NPTS and MISS are written on the plot.
 */
	c_hstopl("PM=ON");
/*
 *  Turn on the printing of the option list.
 */
	c_hstopl("LI=ON");
/*
 *  Print a main title.
 */
	c_hstopc("TIT=ON","Demonstration plot for Histogram.  Special value = -999.",0,0);
	c_histgr((float *)dat1, NDIM, npts, iflag, class, nclass, work, NWRK);
/*
 *     Plot 2:   IFLAG = 1, input values are sorted into a defined set of
 *               8 classes.
 */
	iflag = 1;
	nclass = 8;
	class[0] = -0.6;
	for( i = 0; i < nclass; i++ ) {
        class[i+1] = class[i] + 0.20;
	}
/*
 *  Create some input data.
 */
	x = 0.;
	for( i = 0; i < npts; i++ ) {
        dat1[0][i] = sin(x);
        x = x + .02;
	}
/*
 * Put in some special values
 */
	dat1[0][6] = -999.;
	dat1[0][43]= -999.;
	dat1[0][78]= -999.;

	for( l = 99; l < 220; l++ ) {
		dat1[0][l] = -999.;
	}
/*
 *  (First call HSTOPL("DEF=ON") to activate all default options.)
 */
	c_hstopl("DE=ON");
/*
 *  Then call HSTOPR to activate special value checking
 */
	array[0] = -999.;
	array[1] = 1.e-6;
	c_hstopr("MV=ON",array,2);
/*
 *  Call HSTOPL with NMV = OFF so normalization is relative to NPTS.
 */
	c_hstopl("NM=OF");
/*
 *  Call HSTOPL with PMV = ON, so NPTS and MISS are written on the plot.
 */
	c_hstopl("PM=ON");

	c_hstopc("TIT=ON","Demonstration plot for Histogram.  Normalize to the original number of points.",0,0);
	c_histgr((float *)dat1, NDIM, npts, iflag, class, nclass, work, NWRK);
/*
 *     Plot 3:   IFLAG = 1, define the CLASS values so as to capture
 *               the missing value counts.
 */
	iflag = 1;
	nclass = 7;
/*
 *  Set the first class bin between -1000. and -0.6.  Missing values
 *   of -999. will fall into this bin.
 */
	class[0] = -1000.;
	class[1] = -0.6;
	for( i = 1; i < nclass; i++ ) {
		class[i+1] = class[i] + 0.30;
	}
	npts = 320;

	for( i = 0; i < npts; i++ ) {
		for( j = 0; j < 2; j++ ) {
			dat1[j][i]=0.;
		}
	}
/*
 *  Define some data
 */
	x = 0.;
	for( i = 0; i < npts; i++ ) {
        dat1[0][i] = sin(x);
        x = x + .02;
	}
/*
 * Put in some special values
 */
	dat1[0][6]  = -999.;
	dat1[0][43] = -999.;
	dat1[0][78] = -999.;

	for( l = 199; l < 320; l++ ) {
		dat1[0][l] = -999.;
	}
/*
 *  (First call HSTOPL("DEF=ON") to activate all default options.)
 */
	c_hstopl("DE=ON");
/*
 *  Label the histogram bar endpoints
 */
	c_hstopl("MI=OF");
/*
 *  Convert the class label format to F7.1
 */
	c_hstopc("FOR=ON","(F7.1)",9,7);
/*
 *  To see the missing values in a histogram bar.  Choose CLASS
 *    values so that the missing value indicator becomes a bar
 *    and use MVA = OFF (default.)
 */
	c_hstopc("TIT=ON","Demo plot for special value in Histogram. Title length has been increased to 96 characters.",0,0);

	c_histgr((float *)dat1, NDIM, npts, iflag, class, nclass, work, NWRK);

	printf( "HISTGR TEST SUCCESSFUL\nSEE PLOT TO VERIFY PERFORMANCE\n");
/*
 * Close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}
