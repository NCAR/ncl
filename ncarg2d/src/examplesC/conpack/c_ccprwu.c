#include <stdio.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>

/*
** Define error file, Fortran unit number, and workstation type,
** and workstation ID.
*/
#define IERRF  "stdout"
#define LUNIT  "gmeta"
#define IWTYPE SED_WSTYPE
#define IWKID  1
#define ISZDM  0

#define K 40            /* rows of data */
#define N 40            /* columns of data */
#define LRWK 2000       /* size of real work array */
#define LIWK 2000       /* size of integer work array */


int main()
{
	int i;			/* counter */
        int iwrk[LIWK];         /* integer work array */
	int ncl;		/* number of contour levels */
	int iiwu;		/* integer work space */
	int irwu;		/* real work space */

        float rwrk[LRWK];       /* real work array */
        float z[K][N];          /* contour data */

				/* Create contour data */
	extern void getdat(float*,int,int);


	getdat (&z[0][0], K, N); 

/* Open GKS */
        gopen_gks(IERRF, ISZDM);
        gopen_ws(IWKID, LUNIT, IWTYPE);
        gactivate_ws(IWKID);
        gset_clip_ind (0);

/* Initialize Conpack */
        c_cprect (&z[0][0], N, N, K, rwrk, LRWK, iwrk, LIWK);

/* Draw perimeter */
        c_cpback (&z[0][0], rwrk, iwrk);

/* Draw Contours */
        c_cpcldr (&z[0][0], rwrk, iwrk);
 
/* Find out how much real and integer workspace is used */
      c_cpgeti("IWU - INTEGER WORKSPACE USAGE",&iiwu);
      c_cpgeti("RWU - REAL WORKSPACE USAGE",&irwu);
      printf("Integer Workspace Used: %d  Real Workspace Used: %d\n",iiwu,irwu);

/* Close frame and close GKS */
        c_frame();
        gdeactivate_ws(IWKID);
        gclose_ws(IWKID);
        gclose_gks();
        return (0);
}


void getdat (float* z, int m, int n)
/*
** Create contour data.
**
** z    - data array
** m,n  - number of rows/columns
*/
{

        int i,j;        /* counters */

/* Create contour levels */
        for(i=1; i<=m; ++i)
        {
                for(j=1; j<=n; ++j)
                {
                        z[(((j-1)*n)+i-1)] = 10.0e-5*(-16.*(float)(i*i*j) +
                                  34.0*(float)(i*j*j) - (float)(6*i) + 93.);
		}
	}
}
