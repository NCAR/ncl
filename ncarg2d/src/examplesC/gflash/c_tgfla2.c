/*
 * $Id: c_tgfla2.c,v 1.1 1994-05-13 14:26:54 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

/*
 * OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
 */

main()
{
	int idum,ierr;

	c_opngks();
/*
 * INVOKE DEMO DRIVER
 */
	tgflas(ierr);
/*
 *     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
	c_clsgks();
}

tgflas(ierror)
int ierror;
{
/*
 * Establish the viewport and window.
 */
      c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
/*
 *  Initialize the GFLASH package.  If using a non-NCAR GKS package
 *  the final argument in the following call should be replaced with
 *  the workstation type for WISS.
 */
      gopen_ws(9,NULL,3);
/*
 *  Create plots.
 *
 *  Frame 1 -- title, circle, and fan.
 *
 */
  c_plchlq(0.5,0.91,"FRAME 1",25.,0.,0.);
  c_gflas4(1,"GNFB01");
  c_gflas3(1);
  c_gflas4(2,"GNFB02");
  c_gflas3(2);
  c_gflas4(4,"GNFB04");
  c_gflas3(4);
  c_frame();

/*
 *  Frame 2 -- fan, title, and square.
 */
  c_gflas3(2);
  c_plchlq(0.5,0.91,"FRAME 2",25.,0.,0.);
  c_gflas4(3,"GNFB03");
  c_gflas3(3);
  c_gflas3(4);
  c_frame();
/*
 *  Frame 3 -- circle, square, fan, and title (note that the change
 *             in window affects the PLCHLQ call, but not the elements
 *             in the buffers -- this illustrates the independent
 *             nature of the FLASH buffers).
 */
  c_set (0.,1.,0.,1.,0.,10.,0.,10.,1);
  c_gflas3(1);
  c_gflas3(3);
  c_gflas3(2);
  c_plchlq(5.0,9.1,"FRAME 3",25.,0.,0.);
  c_gflas3(4);
  c_frame();
/*
 *  Close the GFLASH package.
 */
  printf("\n TEST COMPLETE FOR GFLAS4 - see gmeta file to varify");
  printf(" \n  (If blank plots appear, need to run tgflasc first.)\n\n ");
  gclose_ws(9);
}

/*
 * PURPOSE                To provide a simple demonstration of the
 *                        GFLASH package.
 *
 * USAGE                  CALL TGFLAS (IERROR)
 *
 * ARGUMENTS
 *
 * ON OUTPUT              IERROR
 *                          An integer variable
 *                            = 0  If there is a normal exit from GFLASH,
 *                            = 1  Otherwise
 *
 * I/O                    If there is a normal exit from GFLASH,
 *                        the message
 *
 *                          GFLASH TEST SUCCESSFUL . . . SEE PLOTS TO
 *                          VERIFY PERFORMANCE
 *
 *                        is written on unit 6
 *
 * PRECISION              SINGLE
 *
 * REQUIRED LIBRARY       GFLASH
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
 * NOTE                   The call to GOPWK will have to be modified
 *                        when using a non-NCAR GKS package.  The third
 *                        argument must be the workstation type for WISS.
 */








