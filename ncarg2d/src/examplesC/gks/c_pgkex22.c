/*
** illustrate setting nominal linewidth.
**
**
** Define error file, Fortran unit number, and workstation type,
** and workstation iD.
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
#define IWTYPE  22
#define IWKID   1
 

int main()
{
      	float x[2];		/* x coordinate data */
	float y[2];		/* y coordinate data */
	float xp[4];		/* x position data */
	float yp[3];		/* y position data */
	float xlen;		/* length, x direction */
	float sf;		/* line scaling factor */

      	float wscale[5];	/* line width scale */
	float widthn[3];	/* nominal line widths */

	int i; 			/* counter */
	int iscl;		/* line scale loop counter */
	int nwidth;		/* nominal line width counter */

      	char llab[4];		/* label */

        Gcolr_rep color;        /* red, green, and blue color values */

    	Gpoint_list line;	/* line coordinate data */


/*
** Strategic horizontal and vertical positions.
*/ 
      	xp[0]=0.12; xp[1]=0.35; xp[2]=0.60; xp[3]=0.85;
        yp[0]=0.80; yp[1]=0.73; yp[2]=.63;
/*
** Length of displayed lines.
*/
      	xlen = .18;
/*
** Linewidth scale factors.
*/
      	wscale[0]=0.5; wscale[1]=1.0; wscale[2]=2.0; 
        wscale[3]=4.0; wscale[4]=8.0;
/*
** Nominal linewidths.
*/
      	widthn[0]=0.5; widthn[1]=1.0; widthn[2]=4.0;
/*
** Open GKS.
*/
    	gopen_gks (IERRF,0);
/*
** Open and activate a color PostScript workstation.
*/
    	gopen_ws (IWKID, LUNIT, c_ngpswk("PS","PORT","COLOR"));
    	gactivate_ws (IWKID);

        color.rgb.red = 1.0;
        color.rgb.green = 1.0;
        color.rgb.blue = 1.0;
	gset_colr_rep(IWKID, 0, &color);
        color.rgb.red = 0.0;
        color.rgb.green = 0.0;
        color.rgb.blue = 0.0;
	gset_colr_rep(IWKID, 1, &color);
        color.rgb.red = 1.0;
        color.rgb.green = 0.0;
        color.rgb.blue = 0.0;
	gset_colr_rep(IWKID, 2, &color);
/*
** All changes apply to workstation with id IWKID.
*/
      	c_ngseti("Workstation",IWKID);
/*
** Set line caps to "butt".
*/
      	c_ngseti("Caps",0);
 
      	for(nwidth=0; nwidth<3; ++nwidth)
	{
        	x[0] = xp[nwidth+1]-0.5*xlen;
        	x[1] = xp[nwidth+1]+0.5*xlen;
/*
** Set nominal linewidth.
*/
	        c_ngsetr("Nominal linewidth",widthn[nwidth]);
       	 	for(iscl=0; iscl<5; ++iscl)
		{
/*
** Set linewidth scale factor.
*/
			gset_linewidth(wscale[iscl]);
          		y[0] = yp[2] - 0.1*(float)iscl;
          		y[1] = y[0];
/*
** Create structure to pass to gpolyline
*/
            		line.num_points = 2;
            		line.points = (Gpoint *) malloc(4*sizeof(Gfloat));
            		if( !line.points ) 
			{
               	 		fprintf(stderr,"colrln: Not enough memory to ");
				fprintf(stderr,"create fill area structure\n" );
                		gemergency_close_gks();
                		exit(1);
            		}
            		for( i = 0; i < 2; i++ )
			{
                		line.points[i].x = x[i];
                		line.points[i].y = y[i];
            		}
/*
** Draw line
*/
            		gpolyline (&line);
            		free(line.points);
		}
	}
/*
** Label the plot.
*/
      	c_pcseti("FN",25);
        c_pcseti("CC",1);
        c_pchiqu(xp[2],yp[0]-0.010,"Nominal linewidths",.030,0.,0.);
        c_pchiqu(xp[1],yp[1]+0.002,"0.5",.023,0.,0.);
        c_pchiqu(xp[2],yp[1]+0.002,"1.0 (default)",.023,0.,0.);
        c_pchiqu(xp[3],yp[1]+0.002,"4.0",.023,0.,0.);
        c_pchiqu(xp[0],yp[0],"Linewidth",.019,0.,0.);
        c_pchiqu(xp[0],0.5*(yp[0]+yp[1]),"scale",.019,0.,0.);
        c_pchiqu(xp[0],yp[1],"factors",.019,0.,0.);
        sf = .25;
        for(i=0; i<5; ++i)
	{
        	sf = 2.*sf;
		sprintf(llab, "%3.1f", sf);
        	c_pchiqu(xp[0],yp[2]-0.1*(float)i,llab,.023,0.,0.);
	}
 
      	c_ngsetr("Nominal linewidth",1.);
      	gset_line_colr_ind(2);
      	gset_linewidth(8.);
      	x[0] = .03;
      	x[1] = .97;
      	y[0] = yp[1]-0.04;
      	y[1] = y[0];
/*
** Create structure to pass to gpolyline
*/
        line.num_points = 2;
        line.points = (Gpoint *) malloc(4*sizeof(Gfloat));
        if( !line.points )
        {
                fprintf( stderr, "colrln: Not enough memory to ");
                fprintf( stderr, "create fill area structure\n" );
                gemergency_close_gks();
                exit(1);
        }
        for( i = 0; i < 2; i++ )
        {
                line.points[i].x = x[i];
                line.points[i].y = y[i];
        }
/*
** Draw line
*/
        gpolyline (&line);
        free(line.points);

      	x[0] = xp[1]-0.70*xlen;
      	x[1] = x[0];
      	y[0] = yp[0]+0.035;
      	y[1] = yp[2]-0.435;
/*
** Create structure to pass to gpolyline
*/
        line.num_points = 2;
        line.points = (Gpoint *) malloc(4*sizeof(Gfloat));
        if( !line.points )
        {
                fprintf( stderr, "colrln: Not enough memory to ");
                fprintf( stderr, "create fill area structure\n" );
                gemergency_close_gks();
                exit(1);
        }
        for( i = 0; i < 2; i++ )
        {
                line.points[i].x = x[i];
                line.points[i].y = y[i];
        }
/*
** Draw line
*/
        gpolyline (&line);
        free(line.points);

      	c_frame();
/*
** Deactivate and close the workstation, close GKS.
*/
        gdeactivate_ws(IWKID);
        gclose_ws(IWKID);
        gclose_gks();
        return 0;
}
