/*
 *	$Id: c_tpltch.c,v 1.2 1994-06-21 15:01:22 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE  SED_WSTYPE
#define WKID    1

main()
{
    int idum, ierr;
/*
 * OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
 */
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, WSTYPE);
    gactivate_ws (WKID);
/*
 * INVOKE DEMO DRIVER
 */
    tpltch(&ierr);
/*
 *     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}
char *msg[5] = { "This is the graphical output from the example named"  ,
                 "'tpltch'.  It demonstrates minimal functioning of the",
                 "package PLOTCHAR.  For a more complete demonstration" ,
                 "of the capabilities of PLOTCHAR, run the example"     ,
                 "named 'epltch'." };

tpltch(ierr)
int *ierr;
{
    int i;
    char stmp[6];
/*
 * LATEST REVISION        November, 1992.
 *
 * PURPOSE                To provide a minimal demo and test of routines
 *                        in the package PLOTCHAR.
 *
 * USAGE                  CALL TPLTCH (IERR)
 *
 * ARGUMENTS
 *
 * ON OUTPUT              IERR
 *                          An integer variable
 *                          = 0, if the test is successful,
 *                          = 1, otherwise
 *
 * I/O                    If the test is successful, the message
 *
 *                        PLOTCHAR TEST EXECUTED--SEE PLOTS TO CERTIFY
 *
 *                        is written on unit 6.
 *
 *                        In addition, one frame is produced.  To
 *                        determine if the test is successful, it is
 *                        necessary to examine that frame.
 *
 * PRECISION              Single
 *
 * REQUIRED PACKAGES      PLOTCHAR, SPPS
 *
 * REQUIRED GKS LEVEL     0A
 *
 * LANGUAGE               FORTRAN
 *
 * ALGORITHM              TPLTCH just writes a simple message on a
 *                        single frame.  For a more complex example
 *                        of the use of PLOTCHAR, see the example
 *                        named "epltch".
 *
 * Do a call to SET which allows the use of fractional coordinates.
 */
	c_set (0.,1.,0.,1.,0.,1.,0.,1.,1);
/* 
 * Write a single frame, using calls to PCHIQU, PCMEQU, and PCLOQU.
 */
	c_pcmequ (.5,.95,"PLOTCHAR DEMONSTRATION",16.,0.,0.);

	c_pchiqu (.5,.77,"DEMONSTRATING THE USE OF CALLS TO PCHIQU",16.,0.,0.);

	for( i = 1; i <= 5; i++ ) {
		c_pchiqu (.1025,.75-.03*(float)i,msg[i-1],.015,0.,-1.);
	}

	c_pcmequ (.5,.52,"DEMONSTRATING THE USE OF CALLS TO PCMEQU",16.,0.,0.);

	for( i = 1; i <= 5; i++ ) {
		c_pcmequ (.1025,.50-.03*(float)i,msg[i-1],.015,0.,-1.);
	}
	c_pcloqu (.5,.27,"DEMONSTRATING THE USE OF CALLS TO PCLOQU",16.,0.,0.);

	for( i = 1; i <= 5; i++ ) {
		c_pcloqu (.1025,.25-.03*(float)i,msg[i-1],.015,0.,-1.);
	}
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Zero the error flag.
 */
	*ierr=0;
/*
 * Write the log message.
 */
	printf( "PLOTCHAR TEST EXECUTED--SEE PLOTS TO CERTIFY\n");
/* 
 * test c_pcgetc
 */
	c_pcsetc ("FC",":");
	c_pcgetc ("FC",stmp,5);
    printf( "c_pcgetc:  stmp should be ':', stmp is really '%s'\n", stmp );
/*
 * Quit.
 */
	return(1);
}
