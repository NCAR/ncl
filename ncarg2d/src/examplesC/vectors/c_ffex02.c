/*
 *  $Id: c_ffex02.c,v 1.1 1994-08-01 14:22:11 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

/*
 * This program requires the input data file 'ffex02.dat'
 * It reads the data from standard input, e.g.: ffex02 < ffex02.dat
 */
#define MSIZE 36
#define NSIZE 33
#define NCLRS 14
float u[NSIZE][MSIZE], v[NSIZE][MSIZE], p[NSIZE][MSIZE];

main()
{
	extern void rddata();
	int i, ifrmno, iclrix;
    float p1[2],p2[2],p3[2],p4[2];
	float xdm, ydm;
	float vmn, vmx, dmx, vl, vr, vb, vt, ul, ur, ub, ut;
	int idm, ll;
/*
 * Define a set of RGB color triples
 */
	Gcolr_rep rgb[NCLRS];

	rgb[0].rgb.red = 0.0;      rgb[0].rgb.green = 1.0;       rgb[0].rgb.blue = 1.0;
	rgb[1].rgb.red = 0.0745098;rgb[1].rgb.green = 0.92549;   rgb[1].rgb.blue = 0.92549;
	rgb[2].rgb.red = 0.152941; rgb[2].rgb.green = 0.847059;  rgb[2].rgb.blue = 0.847059;
	rgb[3].rgb.red = 0.231373; rgb[3].rgb.green = 0.768627;  rgb[3].rgb.blue = 0.768627;
	rgb[4].rgb.red = 0.305882; rgb[4].rgb.green = 0.694118;  rgb[4].rgb.blue = 0.694118;
	rgb[5].rgb.red = 0.384314; rgb[5].rgb.green = 0.615686;  rgb[5].rgb.blue = 0.615686;
	rgb[6].rgb.red = 0.462745; rgb[6].rgb.green = 0.537255;  rgb[6].rgb.blue = 0.537255;
	rgb[7].rgb.red = 0.537255; rgb[7].rgb.green = 0.462745;  rgb[7].rgb.blue = 0.462745;
	rgb[8].rgb.red = 0.615686; rgb[8].rgb.green = 0.384314;  rgb[8].rgb.blue = 0.384314;
	rgb[9].rgb.red = 0.694118; rgb[9].rgb.green = 0.305882;  rgb[9].rgb.blue = 0.305882;
	rgb[10].rgb.red = 0.768627;rgb[10].rgb.green = 0.231373; rgb[10].rgb.blue = 0.23137;
	rgb[11].rgb.red = 0.847059;rgb[11].rgb.green = 0.152941; rgb[11].rgb.blue = 0.152941;
	rgb[12].rgb.red = 0.92549; rgb[12].rgb.green = 0.0745098;rgb[12].rgb.blue = 0.074509;
	rgb[13].rgb.red = 1.0;     rgb[13].rgb.green = 0.0;      rgb[13].rgb.blue = 0.0;
/*
 *  Open GKS, open and activate a workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws(WKID, NULL, WSTYPE);
    gactivate_ws(WKID);
/*
 * Read the input array data
 */
	rddata();
/*
 * Set up the EZMAP projection
 */
    p1[0] = 60.;
    p2[0] = -120.;
    p3[0] = 23.;
    p4[0] = -60.;
    p1[1] = p2[1] = p3[1] = p4[1] = 0.;
	c_mapset("CO",p1,p2,p3,p4);
	c_maproj("LC",0.0,-75.0,45.0);
	c_mapint();
/*
 * Tell Vectors to use the mapping established by EZMAP
 */
	c_vvseti("MAP -- Mapping Flag", 1);
	c_vvseti("SET -- Set Call Flag", 0);
/*
 * Set up data coordinate boundaries and special value processing 
 * appropriately for the dataset 
 */
	c_vvsetr("XC1 -- Lower X Bound", -140.0);
	c_vvsetr("XCM -- Upper X Bound", -52.5);
	c_vvsetr("YC1 -- Lower X Bound", 20.0);
	c_vvsetr("YCN -- Upper Y Bound", 60.0);

	c_vvseti("SVF -- Special Values Flag", 3);
	c_vvsetr("USV -- U Special Value", -9999.0);
	c_vvsetr("VSV -- V Special Value", -9999.0);
	c_vvsetr("PSV - P Special Value", -9999.0);
	c_vvseti("SPC - P Special Color", 1);
/*
 * Turn on statistics reporting
 */
	c_vvseti("VST -- Vector Statistics", 1);

	for( ifrmno = 1; ifrmno <= 4; ifrmno++ ) {
/*
 * Draw the map with a grid
 */
		c_maplot();
		c_mapgrd();
/*
 * Set up color processing
 */
		if (ifrmno == 4) {
            c_vvseti("CTV -- Color Thresholds Value", 2);
            c_vvseti("NLV -- Number Of Levels", NCLRS);
            for( i = 1; i <= NCLRS; i++ ) {
				iclrix=2+(i-1)*200/NCLRS;
				gset_colr_rep(WKID,iclrix,&rgb[i-1]);
				c_vvseti("PAI -- Parameter Array Index", i);
				c_vvseti("CLR -- GKS Color Index", iclrix);
			}
		}
/*
 * Initialize Vectors
 */
		if (ifrmno > 1) {
            c_vvinit ((float *)u,MSIZE,(float *)v,MSIZE,(float *)p,MSIZE,MSIZE,NSIZE,&xdm,idm);
         }
/*
 * Adjust vector rendering options
 */
         if (ifrmno == 3) {
			 c_vvsetr("AMN -- Arrow Minimum Size",0.007);
			 c_vvsetr("LWD -- Vector Line Width",1.75);
			 c_vvgetr("VMN -- Minimum Vector",&vmn);
			 c_vvgetr("VMX -- Maximum Vector",&vmx);
			 c_vvsetr("VLC -- Vector Low Cutoff",vmn+0.125*(vmx-vmn));
			 c_vvgetr("DMX -- Device Maximum Vector Length",&dmx);
			 c_getset(&vl,&vr,&vb,&vt,&ul,&ur,&ub,&ut,&ll);
			 c_vvsetr("VRL - Vector Realized Length",1.8*dmx/(vr-vl));
			 c_vvsetr("VFR -- Vector Fractional Minimum",0.33);
         }
/*
 * Draw the vector field plot
 */
         if (ifrmno > 1) {
			 c_vvectr ((float *)u,(float *)v,(float *)p,&idm,0,&ydm);
         }
/*
 * Draw a perimeter boundary and eject the frame
 */
		c_perim(1,0,1,0);
		c_frame();
	}
/*
 *     Deactivate and close workstation, close GKS.
 */
    gdeactivate_ws (WKID);
    gclose_ws(WKID);
    gclose_gks();
}

void rddata()
{
	char ch;
	int i, j;
/*
 * Read the data arrays from the standard input 
 */
	for( j = 0; j < NSIZE; j++ ) {
		for( i = 0; i < MSIZE; i++ ) {
			fscanf( stdin, "%g", &u[j][i]);
			fscanf( stdin, "%c", &ch );
		}
	}
	for( j = 0; j < NSIZE; j++ ) {
		for( i = 0; i < MSIZE; i++ ) {
			fscanf( stdin, "%g", &v[j][i]);
			fscanf( stdin, "%c", &ch );
		}
	}
	for( j = 0; j < NSIZE; j++ ) {
		for( i = 0; i < MSIZE; i++ ) {
			fscanf( stdin, "%g", &p[j][i]);
			fscanf( stdin, "%c", &ch );
		}
	}
	return;
}
