#include <stdio.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>

/*
**  Define error file, Fortran unit number, and workstation type,
**  and workstation ID.
*/
#define IERRF  "stdout"
#define LUNIT  "gmeta"
#define IWTYPE SED_WSTYPE
#define IWKID  1
#define ISZDM  0

#define LWRK 3500	/* size of real work array */
#define LIWK 3500	/* size of integer work array */
#define M 40		/* rows of data */
#define N 40 		/* columns of data */


int main()
{
        int iwrk[LWRK];		/* integer work array */

	float y;		/* y coordinate of title */
	float size;		/* size of title chars */
        float zdat[M][N];	/* data array */
	float rwrk[LWRK];	/* real work array */

				/* c_arscam variables */
	float vpl,vpr,vpb,vpt,wl,wr,wb,wt;	
	int log;

				/* Get input data */
	extern void getdat (float*);


	getdat (&zdat[0][0]);
/*
**  Open GKS.
*/
        gopen_gks(IERRF, ISZDM);
        gopen_ws(IWKID, LUNIT, IWTYPE);
        gactivate_ws(IWKID);
        gset_clip_ind (0);
/*
** Initialize Conpack.
*/
        c_cprect (&zdat[0][0],M,M,N,rwrk,LWRK,iwrk,LWRK);

/* Draw Perimeter */
        c_cpback(&zdat[0][0], rwrk, iwrk);

/* Draw Contours */
        c_cpcldr (&zdat[0][0],rwrk,iwrk);

/* Draw a Title */
        c_getset(&vpl,&vpr,&vpb,&vpt,&wl,&wr,&wb,&wt,&log);
        size = .5 * (1.0 - vpt);
	if (size > .017)
        	size = .017;
        y = (1.0+vpt)/2.0+.017;
	if (y > (.017+vpt))
        	y = .017+vpt;
        c_set (0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1);
        c_pchiqu (.5, y, "An Interesting Data Field",size, 0., 0.);
/*
** Advance the frame.
*/
        c_frame();
/*
** Close GKS.
*/
        gdeactivate_ws(IWKID);
        gclose_ws(IWKID);
        gclose_gks();
        return (0);
/*
** Done.
*/
}


void getdat (float* zdat)
/*
** Get input data.
**
** zdat	- 	data array
*/
{
	int i,j;	/* counters */

	FILE* fp;	/* data file pointer */

/*
** Read input data.
*/
	fp = fopen("ccpex.dat","r");
	for(i=0; i<M; ++i)
	{
		for(j=0; j<N; ++j)
		{
			fscanf(fp,"%f",&zdat[((j*N)+i)]);
		}
	}
	fclose(fp);
}
