/*
 *  $Id: c_stex03.c,v 1.1 1994-07-28 14:48:15 haley Exp $
 */
#include <stdio.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define M  25
#define N  25

main()
{
	int i, j, iers, idm;
	float xdm, ydm;
    float plm1[2], plm2[2], plm3[2], plm4[2];
	extern void dfclrs();
/*
 * Example STEX03 draws a uniform southwesterly field on ten
 * different EZMAP projections. A streamline representation overlays
 * a rendering using vectors. Polar input mode is employed: all members
 * of the U array, representing magnitude, are set to 1.0, while the
 * V array contains the directional component, -135.0 degrees.
 *
 * All projections use the maximum possible extent of the globe, except
 * except for frame 3, a Lambert Conical projection, for which a full
 * globe projection is impossible.
 */
	float a[N][M],b[N][M],wrk[M*N*2];
/*
 * Open GKS, open workstation, activate workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws(WKID, NULL, WSTYPE);
    gactivate_ws(WKID);
/*
 * Generate the polar input mode component arrays.
 */
	for( i = 0; i < M; i++ ) {
		for( j = 0; j < N; j++ ) {
            a[j][i] = 1.0;
            b[j][i] = -135.0;
		}
	}
/*
 * Set up a GKS color table
 */
	 dfclrs();
/*
 * Do 10 different EZMAP projections, with Vectors and Streamlines
 * superimposed
 */
	for( i = 1; i <= 10; i++ ) {
/*
 * Draw the map projections
 */
		if (i == 3) {
			plm1[0] = 90.; plm2[0] = 80.; plm3[0] = plm4[0] = 0.;
            c_supmap (3,0.,80.,70.,plm1,plm2,plm3,plm4,2,20,4,0,&iers);
		}
		else {
			plm1[0] = plm2[0] = plm3[0] = plm4[0] = 0.;
            c_supmap (i,0.,0.,0.,plm1,plm2,plm3,plm4,1,20,2,0,&iers);
		}
/*
 * Set the Vectors coordinate parameters appropriately for a full
 * globe polar input mode dataset projected using EZMAP
 */
		c_vvseti("MAP -- Mapping Flag", 1);
		c_vvseti("SET -- Set Call Flag", 0);
		c_vvsetr("XC1 -- Lower X Bound", -180.0);
		c_vvsetr("XCM -- Upper X Bound", 180.0);
		c_vvsetr("YC1 -- Lower Y Bound", -90.0);
		c_vvsetr("YCN -- Upper Y Bound", 90.0);
		c_vvseti("PLR -- Vector Polar Flag", 1);
/*
 * Set the Streamlines coordinate parameters appropriately for a full
 * globe polar input mode dataset projected using EZMAP
 */
		c_stseti("MAP -- Mapping Flag", 1);
		c_stseti("SET -- Set Call Flag", 0);
		c_stsetr("XC1 -- Lower X Bound", -180.0);
		c_stsetr("XCM -- Upper X Bound", 180.0);
		c_stsetr("YC1 -- Lower Y Bound", -90.0);
		c_stsetr("YCN -- Upper Y Bound", 90.0);
		c_stseti("PLR -- Vector Polar Flag", 1);
/*
 * Draw the Vectors in one color
 */
		gset_line_colr_ind(3);
		c_vvinit((float *)a,M,(float *)b,M,&xdm,M,M,N,&ydm,M);
		c_vvectr((float *)a,(float *)b,&xdm,&idm,0,&ydm);
/*
 * Draw the Streamlines in another color
 */
		gset_line_colr_ind(7);
		c_stinit((float *)a,M,(float *)b,M,&xdm,idm,M,N,wrk,2*M*N);
		c_stream((float *)a,(float *)b,&xdm,&idm,0,wrk);
/*
 * Reset the color to the default color index and advance the frame.
 */
		gset_line_colr_ind(1);
		c_frame();
	}
/*
 *     Deactivate and close workstation, close GKS.
 */
    gdeactivate_ws (WKID);
    gclose_ws(WKID);
    gclose_gks();
}

#define NCLRS   16

void dfclrs()
{
	int i;
/*
 * Define a set of RGB color triples for colors 0 through 15.
 */
	Gcolr_rep rgb[NCLRS];
/*
 * Define the RGB color triples needed below.
 */
	rgb[0].rgb.red = 0.00; rgb[0].rgb.green = 0.00; rgb[0].rgb.blue = 0.00;
	rgb[1].rgb.red = 1.00; rgb[1].rgb.green = 1.00; rgb[1].rgb.blue = 1.00;
	rgb[2].rgb.red = 0.70; rgb[2].rgb.green = 0.70; rgb[2].rgb.blue = 0.70;
	rgb[3].rgb.red = 0.75; rgb[3].rgb.green = 0.50; rgb[3].rgb.blue = 1.00;
	rgb[4].rgb.red = 0.50; rgb[4].rgb.green = 0.00; rgb[4].rgb.blue = 1.00;
	rgb[5].rgb.red = 0.00; rgb[5].rgb.green = 0.00; rgb[5].rgb.blue = 1.00;
	rgb[6].rgb.red = 0.00; rgb[6].rgb.green = 0.50; rgb[6].rgb.blue = 1.00;
	rgb[7].rgb.red = 0.00; rgb[7].rgb.green = 1.00; rgb[7].rgb.blue = 1.00;
	rgb[8].rgb.red = 0.00; rgb[8].rgb.green = 1.00; rgb[8].rgb.blue = 0.60;
	rgb[9].rgb.red = 0.00; rgb[9].rgb.green = 1.00; rgb[9].rgb.blue = 0.00;
	rgb[10].rgb.red = 0.70;rgb[10].rgb.green = 1.00;rgb[10].rgb.blue = 0.00;
	rgb[11].rgb.red = 1.00;rgb[11].rgb.green = 1.00;rgb[11].rgb.blue = 0.00;
	rgb[12].rgb.red = 1.00;rgb[12].rgb.green = 0.75;rgb[12].rgb.blue = 0.00;
	rgb[13].rgb.red = 1.00;rgb[13].rgb.green = 0.38;rgb[13].rgb.blue = 0.38;
	rgb[14].rgb.red = 1.00;rgb[14].rgb.green = 0.00;rgb[14].rgb.blue = 0.38;
	rgb[15].rgb.red = 1.00;rgb[15].rgb.green = 0.00;rgb[15].rgb.blue = 0.00;
/*
 * Define 16 different color indices, for indices 0 through 15.  The
 * color corresponding to index 0 is black and the color corresponding
 * to index 1 is white.
 */
	for( i = 0; i < NCLRS; i++ ) {
		gset_colr_rep (WKID,i,&rgb[i]);
	}
	return;
}
