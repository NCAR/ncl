/*
 *	$Id: c_fslfont.c,v 1.1 1994-08-02 20:27:26 haley Exp $
 */
#include <stdio.h>
#include <math.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

/*
 * Store character strings in array CARDS.  These strings contain text,
 * plus information regarding character size and location of the text
 * on the scroll.
 */
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
	extern void fslfnt();
	int ierr;
/*
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Invoke demo driver
 */
	fslfnt(&ierr);
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void fslfnt (ierror)
int *ierror;
{
	int ncards;
	int ierr;
	int nyst, nyfin, movie;
	float tst, tmv, tfin;
/*
 * PURPOSE                To provide a simple demonstration of the
 *                        routine FSLFNT.
 *
 * USAGE                  CALL FSLFNT (IERROR)
 *
 * ARGUMENTS
 *
 * ON OUTPUT              IERROR
 *                          An integer variable
 *                            = 0  If there is a normal exit from FSLFNT
 *                            = 1  Otherwise
 *
 * I/O                    If there is a normal exit from FSLFNT,
 *                        the message
 *
 *                          FSLFNT TEST SUCCESSFUL . . . SEE PLOTS TO
 *                          VERIFY PERFORMANCE
 *
 *                        is written on unit 6
 *
 * PRECISION              SINGLE
 *
 * REQUIRED LIBRARY       FSLFNT
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
	*ierror = 1;
	ncards = 4;
/*
 * Employ the new high quality filled fonts in PLOTCHAR
 */
	c_pcsetc("FN","times-roman");
/*
 * Define the remaining inputs to routine STITLE.  Note that the
 * output produced (a single frame with no scrolling to appear for
 * 6.0 seconds) could equally well have been produced by FTITLE.
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
	c_stitle (cards,ncards,nyst,nyfin,tst,tmv,tfin,movie);
	*ierror = 0;

	printf( "FSLFNT test successful. \n See plots to verify performance.\n");
	return;
}
