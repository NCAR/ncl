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
#define LRWK 1000       /* size of real work array */
#define LIWK 1000       /* size of integer work array */


int main()
{
        int i;                  /* counter */
        int iwrk[LIWK];         /* integer work array */
	int nocl;		/* number of contour levels */

        float rwrk[LRWK];       /* real work array */
        float zreg[K][N];          /* contour data */

        float CIT[10];          /* contour interval table */
        float LIT[10];          /* label interval table */

				/* Create contour data */
	extern void getdat(float*,int,int);


        getdat (&zreg[0][0], K, N);

/* Open GKS */
        gopen_gks(IERRF, ISZDM);
        gopen_ws(IWKID, LUNIT, IWTYPE);
        gactivate_ws(IWKID);
        gset_clip_ind (0);


	c_cpseti("CLS - CONTOUR LEVEL SELECTION FLAG",-20);
/* Initialize Conpack */
        c_cprect(&zreg[0][0], N, N, K, rwrk, LRWK, iwrk, LIWK);
/* Force contour lines to be chosen */
	c_cppkcl (&zreg[0][0], rwrk, iwrk);
/* Get the number of contour levels chosen */
	c_cpgeti("NCL - NUMBER OF CONTOUR LEVELS",&nocl);
	c_cpseti("NCL - NUMBER OF CONTOUR LEVELS",nocl+1);
	c_cpseti("PAI - PARAMETER ARRAY INDEX",nocl+1);
	c_cpsetr("CLV - CONTOUR LEVEL VALUE",0.0);

/* Draw perimeter */
        c_cpback (&zreg[0][0], rwrk, iwrk);
/* Draw Contours */
        c_cpcldr (&zreg[0][0], rwrk, iwrk);

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
