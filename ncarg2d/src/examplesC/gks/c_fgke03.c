/*
 *	$Id: c_fgke03.c,v 1.1 1994-07-27 15:55:11 haley Exp $
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

main()
{
	Gpoint_list line;
    Gescape_in_data in_data;
/*
 *  Open GKS.
 */
	gopen_gks("stdout",0);
	line.num_points = 1;
	line.points = (Gpoint *)malloc(line.num_points*sizeof(Gpoint));
/*
 *  Open and activate a metafile with the name META01.
 */
    in_data.escape_r1.data = (Gdata *)malloc(80*sizeof(char));
	strcpy( in_data.escape_r1.data, "META01" );
    in_data.escape_r1.size = 6;
	gescape(-1391,&in_data,NULL,NULL);
	gopen_ws(WKID,NULL,WSTYPE);
	gactivate_ws (WKID);
/*
 *  Draw a single polymarker in the center of the picture.
 */
	line.points[0].x = line.points[0].y = .5;
	gpolymarker(&line);
	c_frame();
/*
 *  Deactivate and close the META01 metafile.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
/*
 *  Open and activate META02.
 */
	strcpy( in_data.escape_r1.data, "META02" );
	gescape(-1391,&in_data,NULL,NULL);
	gopen_ws(WKID,NULL,WSTYPE);
	gactivate_ws(WKID);
/*
 *  Draw a single polymarker in the upper half of the picture.
 */
	line.points[0].y = .75;
	gpolymarker(&line);
	c_frame();
/*
 *  Deactivate and close the META02 metafile.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
/*
 *  Close GKS.
 */
	gclose_gks();
}
