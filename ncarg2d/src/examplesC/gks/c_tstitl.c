/*
 *	$Id: c_tstitl.c,v 1.2 1994-06-21 15:00:56 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

char *cards[4] =
{
    "  512  760    1  1.5Demonstration                                               ",
    "  512  600    1  1.5Plot                                                        ",
    "  512  440    1  1.0for                                                         ",
    "  512  280    1  1.5STITLE                                                      "
};

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
    int ierr, idum;
/*
 * OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
 */
    gopen_gks ("stdout",0);
    gopen_ws( WKID, NULL, WSTYPE);
    gactivate_ws( WKID );
/*
 * INVOKE DEMO DRIVER
 */
    tstitl(&ierr);
/*
 *     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

tstitl (ierror)
int *ierror;
{
/*
 * PURPOSE                To provide a simple demonstration of the
 *                        routine STITLE.
 *
 * USAGE                  CALL TSTITL (IERROR)
 *
 * ARGUMENTS
 *
 * ON OUTPUT              IERROR
 *                          An integer variable
 *                            = 0  If there is a normal exit from STITLE
 *                            = 1  Otherwise
 *
 * I/O                    If there is a normal exit from STITLE,
 *                        the message
 *
 *                          STITLE TEST SUCCESSFUL . . . SEE PLOTS TO
 *                          VERIFY PERFORMANCE
 *
 *                        is written on unit 6
 *
 * PRECISION              SINGLE
 *
 * REQUIRED LIBRARY       STITLE
 * FILES
 *
 * LANGUAGE               FORTRAN
 *
 * HISTORY                Written  by members of the
 *                        Scientific Computing Division of NCAR,
 *                        Boulder Colorado
 *
 * PORTABILITY            FORTRAN 77
 *
 */
    int ncards, nyst, nyfin, movie, ival;
    float tst, tmv, tfin, rval;
/*
 * Initialize the error parameter.
 */
    *ierror = 1;
/*
 * Define the remaining inputs to routine STITLE.  Note that the
 * output produced (a single frame with no scrolling to appear for
 * 6.0 seconds) could equally well have been produced by MTITLE.
 * We call STITLE in this demo to avoid reading the input lines.
 */
    nyst  = 512;
    nyfin = 512;
    tst   = 0.0;
    tmv   = 0.0;
    tfin  = 6.0;
    movie =   1;
/*
 * Call STITLE.
 */
    ncards = 4;
    c_stitle (cards,ncards,nyst,nyfin,tst,tmv,tfin,movie);
    *ierror = 0;
    printf("     STITLE TEST SUCCESSFUL\nSEE PLOTS TO VERIFY PERFORMANCE\n");
/*
 * Test c_slgetr
 */
    c_slsetr("FIN",1.5);
    c_slgetr("FIN",&rval);
    printf( "\nc_slgetr, c_slgetr: rval should be 1.5, rval is really %g\n", rval );
/*
 * Test c_slgeti, c_slseti
 */
    c_slseti("ALN",3);
    c_slgeti("ALN",&ival);
    printf( "\nc_slgeti, c_slgeti: ival should be 3, ival is really %d\n", ival );
    return(1);
}
