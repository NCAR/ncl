#include <math.h>
#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define IWTYPE   1

main()
{
    int i, lstr;
    float tright, xstart, xend, xin, xout, ypos, top, bot;
	extern void drwbod();
/*
 *  A plot illustrating the text extent rectangle.
 */
	char string[14], ch[2];
	strcpy( string, "NCAR Graphics" );
/*
 *  Open GKS, open and activate a CGM workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws( WKID, NULL, WSTYPE);
    gactivate_ws( WKID );
/*
 *  Specify the text font and color for Plotchar.
 */
	c_pcseti("FN",21);
	c_pcseti("CC",1);
/*
 *  Calculate the initial X start point to center the string.
 */
	c_pcseti("TE",1);
	c_plchhq(0.,.5,string,.1*6./7.,360.,-1.);
	c_pcseti("TE",0);
	c_pcgetr("XE",&tright);
    xstart = .5*(1.-tright);
/*
 *  Draw the characters and boxes around the character bodies.
 */
    lstr = strlen(string);
    xin  = xstart;
    ypos = .475;
    for( i = 1; i <= lstr; i++ ) {
		ch[0] = string[i-1];
		ch[1] = '\0';
        drwbod(xin,ypos,ch,.1,&xout,top,bot);
        xin = xout;
    }
    xend = xout;
/*
 *  Label the plot.
 */
    c_pcseti("FN",25);
    c_plchhq(.5,.80,"Text Extent Rectangle",.040,0.,0.);
    c_plchhq(.5,.72, "A concatenation of character bodies",.035,0.,0.);
    c_pcseti("FN",21);
    c_plchhq(.5,.64,"The hatched area shades the text extent rectangle",.025,0.,0.);
    c_frame();
/*
 *  Deactivate and close the workstation, close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

void drwbod(xin,y,chr,chgt,xout,top,bot)
float xin, *xout, top, bot, y, chgt;
char *chr;
{
/*
 *  Draw the character in CHR left-centered at (XIN,Y) with character
 *  height CHGT.  Draw a box around the character body.  Return the 
 *  right of the character body in XOUT and the font top and bottom
 *  in TOP and BOTTOM.
 */
	float cap, base, half, cwidth, tright, tleft;
	Gpoint_list line;

	line.num_points = 5;
	line.points = (Gpoint *)malloc(line.num_points*sizeof(Gpoint));
	if( !line.points ) {
		fprintf( stderr, "drwbod: Not enough memory to create line structure\n" );
		gemergency_close_gks();
		exit(1);
	}

	cap =  y+.5*chgt;
	base = y-.5*chgt;
	half = .5*(cap+base);
	top = half+.7*(cap-base);
	bot = half-.8*(cap-base);
/*
 *  Convert the character height to width for Plotchar.
 */
	cwidth = (6./7.)*(cap-base);
/*
 *  Compute the text extent information.
 */
	c_pcseti("TE",1);
	c_plchhq(xin,half,chr,cwidth,360.,-1.);
	c_pcseti("TE",0);
	c_pcgetr("XB",&tleft);
	c_pcgetr("XE",&tright);
/*
 *  Draw a box around the character body limits and hatch the interior.
 */
	line.points[0].x = tleft;
	line.points[1].x = tright;
	line.points[2].x = tright;
	line.points[3].x = tleft;
	line.points[4].x = line.points[0].x;
	line.points[0].y = bot;
	line.points[1].y = bot;
	line.points[2].y = top;
	line.points[3].y = top;
	line.points[4].y = line.points[0].y;
	gset_linewidth(2.);
	gpolyline(&line);
	gset_fill_int_style(GSTYLE_HATCH);
	gset_fill_style_ind(6);
	gfill_area(&line);
	gset_linewidth(1.);
	gset_fill_int_style(GSTYLE_SOLID);
/*
 *  Draw the character.
 */
	c_plchhq(xin,half,chr,cwidth,0.,-1.);
/*
 *  Return the right limit of the character body.
 */
	*xout = tright;

	return;
}
