/*
 *	$Id: c_fgke02.c,v 1.1 1994-07-27 15:55:10 haley Exp $
 */
/*
 *  Illustrate creating multiple metafiles in the same job.
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

/*
 *  Data for the border and two lines.
 */
float perimx[5] = {0., 1., 1., 0., 0.};
float perimy[5] = {0., 0., 1., 1., 0.};

main()
{
/*
 *  Define error file, Fortran unit number, and workstation type,
 *  and workstation ID.
 *
 * PURPOSE                To provide a simple demonstration of the
 *                        GFLASH package.
 *
 * I/O                    If there is a normal exit from GFLASH,
 *                        the message
 *
 *                          GFLASH TEST SUCCESSFUL . . . SEE PLOTS TO
 *                          VERIFY PERFORMANCE
 *
 *                        is written on unit 6
 *
 * NOTE                   The call to GOPWK will have to be modified
 *                        when using a non-NCAR GKS package.  The third
 *                        argument must be the workstation type for WISS.
 */
	Gpoint_list line;
	int i;

    line.num_points = 5;
    line.points = (Gpoint *)malloc(line.num_points*sizeof(Gpoint));
	line.points[0].x = line.points[0].y = .25;
	line.points[1].x = line.points[1].y = .75;
/*
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID,NULL,WSTYPE);
	gactivate_ws (WKID);
/*
 *  Initialize the GFLASH package.  If using a non-NCAR GKS package
 *  the final argument in the following call should be replaced with
 *  the workstation type for WISS.
 */
      gopen_ws(9,NULL,3);
/*
 *  Establish character height and text alignment.
 *
 *
 *  Put a line with positive slope into flash buffer 1.
 */
	c_gflas1(1);
    line.num_points = 2;
	gpolyline(&line);
	c_gflas2();
/*
 *  Put a line with negative slope into flash buffer 2.
 */
	c_gflas1(2);
	line.points[0].x = line.points[1].y = .75;
	line.points[1].x = line.points[0].y = .25;
	gpolyline(&line);
	c_gflas2();
/*
 *  Draw the border.
 */
    line.num_points = 5;
	for( i = 0; i < 5; i++ ) {
		line.points[i].x = perimx[i];
		line.points[i].y = perimy[i];
	}
	gpolyline(&line);
/*
 *  Put the two segments into the picture.
 */
	c_gflas3(1);
	c_gflas3(2);
	c_frame();
/*
 *  Close the GFLASH package.
 */
	gclose_ws(9);
/*
 *  Deactivate and close the metafile workstation, close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

