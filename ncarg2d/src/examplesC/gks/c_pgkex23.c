/*
** Illustrate changing spacing between hatch pattern lines.
**
**
** Define error file, Fortran unit number, and workstation type,
** and workstation ID.
*/

#include <stdio.h>
#include <math.h>

/*
** Include function prototypes
*/
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>


#define IERRF   "stdout" 
#define LUNIT   0
#define IWKID   1


int main()
{
	int i;
	int ihatch[4];

	char lab[15];

      	float xl[4];
	float xr[4];
 	float yb[4];
	float yt[4];
	float rspace[4];

        Gcolr_rep color;        /* red, green, and blue color values */

	extern void zigzag();

      	xl[0]=0.05; xl[1]=0.55; xl[2]=0.05; xl[3]=0.55;
      	xr[0]=0.45; xr[1]=0.95; xr[2]=0.45; xr[3]=0.95;
      	yt[0]=0.90; yt[1]=0.90; yt[2]=0.40; yt[3]=0.40;
      	yb[0]=0.50; yb[1]=0.50; yb[2]=0.00; yb[3]=0.00;

      	ihatch[0]=1; ihatch[1]=1; ihatch[2]=3; ihatch[3]=6;
      	rspace[0]=0.01; rspace[1]=0.02; rspace[2]=0.03; rspace[3]=0.05;
/*
** Open GKS.
*/
        gopen_gks (IERRF,0);
/*
** Open and activate a color PostScript workstation.
*/
        gopen_ws (IWKID, LUNIT, c_ngpswk("PS","PORT","COLOR"));
        gactivate_ws (IWKID);
/*
** Set up color table.
*/
        color.rgb.red = 1.0;
        color.rgb.green = 1.0;
        color.rgb.blue = 1.0;
        gset_colr_rep(IWKID, 0, &color);
        color.rgb.red = 0.0;
        color.rgb.green = 0.0;
        color.rgb.blue = 0.0;
        gset_colr_rep(IWKID, 1, &color);
        color.rgb.red = 0.0;
        color.rgb.green = 0.0;
        color.rgb.blue = 1.0;
        gset_colr_rep(IWKID, 2, &color);

      	gset_fill_int_style(3);
      	c_ngseti("Workstation ID",IWKID);
      	c_pcseti("FN",25);
      	c_pcseti("CC",1);
	for(i=0; i<4; ++i)
	{
       		gset_fill_int_style(3);
       		gset_fill_style_ind(ihatch[i]);
        	c_ngsetr("Hatch spacing",rspace[i]);
        	zigzag(xl[i],xr[i],yb[i],yt[i]);
        	gset_fill_int_style(1);
		sprintf(lab, "Style index = %1d", ihatch[i]);
        	c_pchiqu(xl[i]+0.21,yt[i]-0.02,lab,0.022,0.,0.);
		sprintf(lab, "Spacing = %4.2f ", rspace[i]);
        	c_pchiqu(xl[i]+0.32,yt[i]-0.09,lab,0.022,0.,0.);
	}

      	c_pchiqu(.5,.98,"Hatch line spacings",0.035,0.,0.);

        c_frame();
/*
** Deactivate and close the workstation, close GKS.
*/
        gdeactivate_ws(IWKID);
        gclose_ws(IWKID);
        gclose_gks();
        return 0;
}

#define IDIM 10

void zigzag(xleft,xright,ybot,ytop)
float xleft,xright,ybot,ytop;
{
/*
** Draw a zigzag figure within the specified limits for a rectangle.
*/
        Gpoint_list line;       /* line coordinate data */

      	float x[IDIM], y[IDIM];
	float dx, dy;

	int i;
 
      	dx = (xright-xleft)/8.;
      	dy = (ytop-ybot)/10.;
 
	for(i=0; i<IDIM-1; ++i)
	{
        	x[i] = xleft+(i*dx);
	}
      	x[IDIM-1] = x[0];
 
      	y[0] = ytop-2.*dy;
      	y[1] = ytop;
      	y[2] = ybot;
      	y[3] = ytop-2.0*dy;
      	y[4] = ybot;
      	y[5] = ytop-4.0*dy;
      	y[6] = ybot;
      	y[7] = ytop-6.0*dy;
      	y[8] = ytop-8.5*dy;
      	y[9] = y[0];
 
/*
** Create structure to pass to gpolyline
*/
        line.num_points = IDIM;
        line.points = (Gpoint *)malloc(2*IDIM*sizeof(Gfloat));
        if( !line.points )
        {
                fprintf( stderr, "colrln: Not enough memory to ");
                fprintf( stderr, "create fill area structure\n" );
                gemergency_close_gks();
                exit(1);
        }
        for( i = 0; i < IDIM; i++ )
        {
                line.points[i].x = x[i];
                line.points[i].y = y[i];
        }
/*
** Draw line
*/
      	gset_fill_colr_ind(2);
      	gfill_area(&line);
      	gset_line_colr_ind(1);
        gpolyline (&line);
        free(line.points);
}
