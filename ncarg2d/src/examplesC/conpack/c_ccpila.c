/*
 * $Id: c_ccpila.c,v 1.2 1994-06-21 14:59:26 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define M  40
#define N  40
#define LRWK  3500
#define LIWK  4000

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	float z[N][M], rwrk[LRWK];
	int iwrk[LIWK];
	extern void getdat();

	getdat (z, M, N);
/*
 * Open GKS
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
	gset_clip_ind(GIND_NO_CLIP);
/*
 * Initialize Conpack
 */
	c_cpsetr("HLA - HIGH/LOW LABEL ANGLE",30.);
	c_cpsetr("ILA - INFORMATION LABEL ANGLE",90.);
	c_cpsetr("ILX - INFORMATION LABEL X COORD",-0.02);
	c_cpsetr("ILY - INFORMATION LABEL Y COORD",0.25);
	c_cpseti("ILP - INFORMATION LABEL POSITION", 0);
	c_cprect((float *)z, M, M, N, rwrk, LRWK, iwrk, LIWK);
/*
 * Draw Perimeter
 */
	c_cpback((float *)z, rwrk, iwrk);
/*
 * Draw Contours
 */
	c_cplbdr((float *)z,rwrk,iwrk);
	c_cpcldr((float *)z,rwrk,iwrk);
/*
 * Close frame and close GKS
 */
	c_frame();
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}


void getdat (z, m, n)
float z[N][M];
int m, n;
{
	int i, j;
	FILE *fp;

	fp = fopen("ccpila.dat","r");
	for( i = 0; i < m; i++ ) {
		for( j = 0; j < n; j++ ) {
			fscanf( fp, "%g", &z[j][i] );
		}
	}
	fclose(fp);
	return;
}
