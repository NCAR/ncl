/*  file: 
**
**  Description:  This program displays a connected graph
**	which are translated about the X axis a number of steps.
**	The steps with clicks are displayed individually,
**	while the generated metafile displays overlapping graphs.
**
**  NCAR graphic subroutines:
**
** 	gopen_gks(const char *err_file, size_t mem_unit);
**     	gopen_ws(Gint ws_id, const char *conn_id, Gint ws_type);
**	gactivate_ws(Gint ws_id);
**	c_plotif (float fx, float fy, int ip);
**     	c_ngpict (int wkid, int action);
**    	gclear_ws (Gint ws_id, Gctrl_flag  ctrl_flag);
**	gdeactivate_ws(Gint ws_id);
**	gclose_ws(Gint ws_id);
**	gclose_gks();
*/

#include <stdio.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>


#define IWTYPE 1
#define WKID   1

/* NCAR graphic dependent definitions */  

#define X_WIN     	2	/* X window identifier */
#define KLICK		2	/* Mouse click? 0=off; 2=on */


/* program dependent definitions */  

#define PIC_PNTS	9		/* node points (line connectors) */	
#define STEPS 		9		/* number of picture transformations */
#define PI 		3.14159		/* PI */


/* Structure containing point X and Y coordinates */

typedef struct {
	float crd[2];		/* x=0,y=1 coordinate points */
} coord;
      

float trans_matrx[2][2];	/* translation matrix */


void main()
{
	int i;				/* counter */

	coord points[PIC_PNTS];		/* node point coordinates */

	void set_up_drawing(coord*);	/* initialize node points */
	void draw_object(coord*);	/* draw line picture */
	void translate(coord*);		/* point translation */	
	void set_trans_matrx(float);	/* initialize translation matrix */


/* open and activate gks xwindow and metafile */

	printf("opening xgks window\n");
 	gopen_gks("stdout", 0);
   	gopen_ws(X_WIN, "1", 8);
   	gopen_ws(WKID, NULL, IWTYPE);
	gactivate_ws(X_WIN);
	gactivate_ws(WKID);

/* set initial drawing and draw it */

	set_trans_matrx(0.1);
	set_up_drawing(points);
	draw_object(points);

/* update gks xwindow and metafile */

   	c_ngpict (X_WIN,KLICK);
   	c_ngpict (WKID,0);

/* start transformation loops */

	for(i=0; i<STEPS; ++i)
	{
   		printf("rendering frame= %2d\n", i+1);
   		gclear_ws (X_WIN,1);

	/* translate and draw object */

		translate(points);		
		draw_object(points);

	/* update gks xwindow and metafile */

   		c_ngpict (X_WIN,KLICK);
   		c_ngpict (WKID,0);
	}

/* close windows */

	gdeactivate_ws(X_WIN);
	gdeactivate_ws(WKID);
 	gclose_ws(X_WIN);
 	gclose_ws(WKID);
 	gclose_gks();
}



/*
** void draw_object(coord* points)
**
** Make a "pen" drawing from all points to all points.
*/

void draw_object
(    	coord* points 	/* point definitions */
)

{
	int i,j;	/* loop counters */

/* Draw lines from point 'i' to point 'j' */

	for (i=0; i<PIC_PNTS; ++i)
	{
	     for (j=i+1; j<PIC_PNTS; ++j)
	     {
		  if(j>3)
		  {
			c_plotif (points[i].crd[0], points[i].crd[1], 0);
			c_plotif (points[j].crd[0], points[j].crd[1], 1);
		  }
	     }
	}
}


/*
** void set_up_drawing(coord* points)
**
** Set X and Y coordinate values to all points.
** Values must be between 0 and 1 inclusive.
*/

void set_up_drawing
(	coord* points	/* point definitions */
)

{
	points[0].crd[0] = 0.5;
	points[0].crd[1] = 0.05;

	points[1].crd[0] = 0.5;
	points[1].crd[1] = 0.95;

	points[2].crd[0] = 1.0;
	points[2].crd[1] = 0.5;

	points[3].crd[0] = 0.0;
	points[3].crd[1] = 0.5;

	points[4].crd[0] = 0.5;
	points[4].crd[1] = 0.5;

	points[5].crd[0] = 0.35;
	points[5].crd[1] = 0.35;

	points[6].crd[0] = 0.65;
	points[6].crd[1] = 0.35;

	points[7].crd[0] = 0.35;
	points[7].crd[1] = 0.65;

	points[8].crd[0] = 0.65;
	points[8].crd[1] = 0.65;
}


/*
** void set_trans_matrx(float x_rad)
**
** Initialize transformation matrix.
*/

void set_trans_matrx
(	float x_rad	/* radians per shift */
)

{
	int i,j;	/* loop counters */
	
/* initialize translation matrix */

	trans_matrx[0][0] = cos((double)x_rad);
	trans_matrx[1][1] = cos((double)x_rad);
	trans_matrx[1][0] = sin((double)x_rad);
	trans_matrx[0][1] = -sin((double)x_rad);
}


/*
** void translate(coord* point)
**
** Translate the point about the X axis.
*/

void translate
(	coord* point	/* point definitions */
)

{
	int j,k,l;			/* loop counters */

	float total;			/* total of matrix multiply */
	float temp[PIC_PNTS][3];	/* temporarily saved changed point */

/* multiply point by rotational matrix */

	for(l=0; l<PIC_PNTS; ++l)
	{
		for(j=0; j<2; ++j)
		{
			total = 0.0;
			for(k=0; k<2; ++k)
			{
			      total+=(point[l].crd[j]-0.5)*trans_matrx[k][j];
			}
			temp[l][j] = total + 0.5;
		}
	}

/* put results back into point */

	for(l=0; l<PIC_PNTS; ++l)
	{
		for(j=0; j<2; ++j)
		{
			point[l].crd[j] = temp[l][j];
		}
	}
}
