#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define min(x,y)   ((x)<(y) ? (x):(y))

main()
{
	float umin, umax, vmin, vmax, wmin, wmax;
	float upos, vpos, wpos, ptmp, qtmp, utmp, vtmp, wtmp;
	int i, j, ilab, ibeg, iend, ichr;
	extern float wfun();
	extern void color();
/*
** Declare an array in which to put an eye position for THREED.
**/
	float peye[3];
/*
** Define a character variable in which to form numeric labels.
**/
	char chrs[9];
/*
**  Open GKS, open and activate a workstation.
**/
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
** Make the tick marks drawn by PERIM3 different from the default.
**/
	c_tick43 (24,16,24,16,24,16);
/*
** Define the boundaries of the box to be projected from 3-space to
** 2-space.
**/
	umin=0.;
	umax=1.;
	vmin=0.;
	vmax=1.;
	wmin=0.;
	wmax=1.;
/*
** Define the position of the eye.
**/
	peye[0] = 6.;
	peye[1] = 4.;
	peye[2] = 5.;
/*
** Initialize THREED.
**/
	c_set3 (.1,.9,.1,.9,umin,umax,vmin,vmax,wmin,wmax,peye);
/*
** Draw perimeters in each of the three coordinate planes.
**/
	c_perim3 (10,2,10,2,1,0.);
	c_perim3 (10,2,10,2,2,0.);
	c_perim3 (10,2,10,2,3,0.);
/*
** Put some labels on the plot.  First, the U axis.
*/
	c_pwrzt (.5,0.,1.1,"U",1,3,-1,+3,0);

	for( ilab = 1; ilab <=10; ilab++ ) 
	{
		upos=(float)(ilab)/10.;
		sprintf( chrs, "%8.1f", upos );
		ibeg=-1;
		for( ichr = 0; ichr < 8; ichr++ ) 
		{
            		if (chrs[ichr] != ' ') 
			{
				if (ibeg == -1) 
				{
					ibeg=ichr;
				}
				iend=ichr;
			}
		}
		if (chrs[ibeg] == '0') ibeg = min(ibeg+1,iend);
		c_pwrzt (upos,0.,1.05,&chrs[ibeg],iend-ibeg+1, 3,-1,3,0);
	}
/*
** next, the v axis.
*/
   	c_pwrzt (0.,.5,1.1,"V",1,3,2,3,0);

	for( ilab = 1; ilab <=10; ilab++ ) 
	{
		vpos=(float)(ilab)/10.;
		sprintf( chrs, "%8.1f", vpos );
		ibeg=-1;
		for( ichr = 0; ichr < 8; ichr++ ) 
		{
            		if (chrs[ichr] != ' ') 
			{
               			if (ibeg == -1) 
				{
				   	ibeg=ichr;
               			}
               			iend=ichr;
		   	}
		}
		if (chrs[ibeg] == '0') ibeg = min(ibeg+1,iend);
		c_pwrzt (0.,vpos,1.05,&chrs[ibeg],iend-ibeg+1, 3,2,3,0);
	}
/*
** Finally, the w axis.
**/
	c_pwrzt (1.2,0.,.5,"W",1,3,-1,3,1);

	for( ilab = 0; ilab <=10; ilab++ ) 
	{
		wpos=(float)(ilab)/10.;
		sprintf( chrs, "%8.1f", wpos );
		ibeg=-1;
		for( ichr = 0; ichr < 8; ichr++ ) 
		{
            		if (chrs[ichr] != ' ') 
			{
				if (ibeg == -1) 
				{
					ibeg=ichr;
				}
				iend=ichr;
            		}
		}
		if (chrs[ibeg] == '0') ibeg = min(ibeg+1,iend);
		c_pwrzt (1.05,0.,wpos,&chrs[ibeg],iend-ibeg+1,3,-1,3,1);
	}
/*
** Using POINT3, draw grids inside the perimeters drawn by PERIM3.
**/
	for( i = 1; i <=11; i++ ) 
	{
		ptmp=(float)(i-1)/10.;
		for( j = 1; j <=101; j++ ) 
		{
            		qtmp=(float)(j-1)/100.;
			c_point3 (ptmp,qtmp,0.);
            		c_point3 (qtmp,ptmp,0.);
            		c_point3 (ptmp,0.,qtmp);
            		c_point3 (qtmp,0.,ptmp);
            		c_point3 (0.,ptmp,qtmp);
            		c_point3 (0.,qtmp,ptmp);
		}
	}
/*
** Draw a wire-frame representation of the surface defined by the
** function wfun, using the routines FRST3 and VECT3.
*/
	for( i = 1; i <=11; i++ ) 
	{
		utmp=(float)(i-1)/10.;
		c_frst3 (utmp,0.,wfun(utmp,0.));
		for( j = 2; j <= 11; j++ ) 
		{
            		vtmp=(float)(j-1)/10.;
            		c_vect3 (utmp,vtmp,wfun(utmp,vtmp));
		}
	}
	for( j = 1; j <= 11; j++ ) 
	{
		vtmp=(float)(j-1)/10.;
		c_frst3 (0.,vtmp,wfun(0.,vtmp));
		for( i = 2; i <= 11; i++ ) 
		{
            		utmp=(float)(i-1)/10.;
            		c_vect3 (utmp,vtmp,wfun(utmp,vtmp));
		}
	}
/*
** Double the line width and put a little set of axes at each point on
** the surface.
*/

	c_plotif (0.,0.,2);
	gset_linewidth (2.);
 
	for(i=1; i<=21; i=i+2) 
	{
         	utmp=(float)(i-1.0)/20.;
		for(j=1; j<=21; j=j+2) 
		{
            		vtmp=(float)(j-1.0)/20.;
            		wtmp=wfun(utmp,vtmp);
            		c_line3 (utmp-.04,vtmp,wtmp,utmp+.04,vtmp,wtmp);
            		c_line3 (utmp,vtmp-.04,wtmp,utmp,vtmp+.04,wtmp);
            		c_line3 (utmp,vtmp,wtmp-.04,utmp,vtmp,wtmp+.04);
		}
	}
/*
** Advance the frame.
**/
	c_frame();
/*
** Deactivate and close workstation, close GKS.
**/
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

/*
** Declare a function W(U,V) to be used in the example.
**/
	
float wfun(u,v)
float u, v;
{
	return((.5+.25*sin(5.*u)+.25*cos(5.*v)));
}
