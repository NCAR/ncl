#include <math.h>
#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define IWTYPE   1

/*
 * Getting a PostScript plot to fill an entire page. 
 */ 

main()
{
	char lab[4];
	int i, j, num;
	float x, y;
	extern void box();
/*
 *  Open GKS.
 */
	gopen_gks("stdout", 0 );
/*
 *  Specify the output position for the next PostScript workstation to be
 *  opened.  These calls must appear before the call to open the workstation.
 */
	c_ngseti("LX", 50);
	c_ngseti("LY", 50);
	c_ngseti("UX",742);
	c_ngseti("UY",742);
/*
 *  Open and activate a color PostScript workstation in landscape mode.
 */
	gopen_ws (WKID, NULL, c_ngpswk("PS","LAND","COLOR"));
	gactivate_ws (WKID);
/*
 *  Draw three rows and four columns of square boxes.
 */
	num = 0;
	for( j = 1; j <= 3; j++ ) {
        y = 0.25*(float)(j-1);
        for( i = 1; i <= 4; i++ ) {
			num++;
			sprintf( lab, "%3d", num );
			x = 0.25*(float)(i-1);
			box(x, y, 0.25, lab);
		}
	}
	c_frame();
/*
 *  Close things out.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void box(x,y,sz,lab)
float x, y, sz;
char *lab;
{
/*
 *  Draw a square box with lower left corner at (X,Y) and size SZ x SZ 
 *  and put the label LAB in the center.
 */
	Gpoint pos;
	Gpoint_list line;
	Gtext_align text_align;

	line.num_points = 5;
	line.points = (Gpoint *)malloc(line.num_points*sizeof(Gpoint));
	if( !line.points ) {
		fprintf( stderr, "box: Not enough memory to create fill area structure\n" );
		gemergency_close_gks();
		exit(1);
	}
/*
 *  Draw box.
 */
	line.points[0].x = x;
	line.points[0].y = y;
	line.points[1].x = x+sz;
	line.points[1].y = y;
	line.points[2].x = line.points[1].x;
	line.points[2].y = y+sz;
	line.points[3].x = x;
	line.points[3].y = line.points[2].y;
	line.points[4].x = x;
	line.points[4].y = y;
	gpolyline(&line);
/*
 *  Write label in box.
 */
	gset_char_ht(0.25*sz);
	text_align.hor = GHOR_CTR;
	text_align.vert = GVERT_HALF;
	gset_text_align(&text_align);
	pos.x = x+.5*sz;
	pos.y = y+.5*sz;
	gtext(&pos, lab);

	return;
}
