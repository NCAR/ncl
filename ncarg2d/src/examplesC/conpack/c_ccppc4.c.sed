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

#define MREG    50      /* rows of contour data */
#define NREG    50      /* columns of contour data */
#define LRWK  3500      /* size of real work array */
#define LIWK  4000      /* size of integer work array */
#define LMAP 75000      /* size of map data array */
#define NRAN    30      /* number of coordinate data values */


int main()
{
        int i;                  /* counter */
        int iwrk[LIWK];         /* integer work array */
        int map[LMAP];          /* map data array */
        int ncons;              /* number of contour levels */

        float x[MREG];          /* X coordinate data values */
        float y[NREG];          /* Y coordinate data values */
        float zreg[MREG][NREG]; /* Z coordinate data values */
        float rwrk[LRWK];       /* real work array */

				/* Get plotting data */
	extern void getdat(float*,float*,float*);

				/* Polyline processing routine */
	extern int cpdrpl_(float*, float*, int*, int*, int*, int*);


        getdat(x, y, &zreg[0][0]);
/* Open GKS */
        gopen_gks(IERRF, ISZDM);
        gopen_ws(IWKID, LUNIT, IWTYPE);
        gactivate_ws(IWKID);
/* Initialize Areas */
        c_arinam(map,LMAP);
/* Choose which labelling scheme will be used. */
        c_cpseti("LLP - LINE LABEL POSITIONING FLAG",3);
/* Set the optimum distance term so that labels are far appart */
        c_cpsetr("PC4 - PENALTY SCHEME CONSTANT 4",0.1);
        c_cpsetr("PC5 - PENALTY SCHEME CONSTANT 5",0.2);
        c_cpsetr("PC6 - PENALTY SCHEME CONSTANT 6",0.05);
        c_cpsetr("PW4 - PENALTY SCHEME WEIGHT 4",5.0);
/* Initialize Conpack */
        c_cprect(&zreg[0][0], NREG, NREG, MREG, rwrk, LRWK, iwrk, LIWK);
/* Force Conpack to chose contour levels */
        c_cppkcl(&zreg[0][0], rwrk, iwrk);
/* Modify Conpack chosen parameters */
        c_cpgeti("NCL - NUMBER OF CONTOUR LEVELS",&ncons);
        for(i=1; i<=ncons; ++i)
        {
           c_cpseti("PAI - PARAMETER ARRAY INDEX",i);
/* Force every line to be labeled. */
           c_cpseti("CLU - CONTOUR LEVEL USE FLAG",3);
        }
/* Add contours to area map */
        c_cpclam(&zreg[0][0],rwrk,iwrk,map);
/* Add labels to area map */
        c_cplbam(&zreg[0][0],rwrk,iwrk,map);
/* Draw Perimeter */
        c_cpback(&zreg[0][0], rwrk, iwrk);
/* Draw Labels */
        c_cplbdr(&zreg[0][0],rwrk,iwrk);
/* Draw Contours */
        c_cpcldm(&zreg[0][0],rwrk,iwrk,map,cpdrpl_);
/* Close frame and close GKS */
        c_frame();
        gdeactivate_ws(IWKID);
        gclose_ws(IWKID);
        gclose_gks();
        return (0);
}


void getdat(float* xreg, float* yreg, float* zreg)
/*
** Initialize data.
**
** xreg, yreg, zreg     - X, Y, and Z coordinate data.
*/
{
        int i;                  /* counter */
        int iwrk[LIWK];         /* integer work array */

        float xran[NRAN];       /* x coordinate data */
        float yran[NRAN];       /* y coordinate data */
        float zran[NRAN];       /* z coordinate data */
        float rwrk[LRWK];       /* real work array */
        float xmin, xmax;       /* x min/max data values */
        float ymin, ymax;       /* y min/man data values */


        xran[0]=12.;   xran[1]= 60.;  xran[2]= 14.;  xran[3]= 33.;
        xran[4]= 8.;   xran[5]= 12.;  xran[6]= 43.;  xran[7]= 57.;
        xran[8]= 22.;  xran[9]= 15.;  xran[10]=19.;  xran[11]= 12.;
        xran[12]= 64.; xran[13]= 19.; xran[14]= 15.; xran[15]= 55.;
        xran[16]= 31.; xran[17]= 32.; xran[18]= 33.; xran[19]= 29.;
        xran[20]=18.;  xran[21]=  1.; xran[22]= 18.; xran[23]= 42.;
        xran[24]= 56.; xran[25]=  9.; xran[26]=  6.; xran[27]= 12.;
        xran[28]= 44.; xran[29]= 19.;

        yran[0]=  1.; yran[1]=  2.; yran[2]=  3.; yran[3]= 53.;
        yran[4]=  7.; yran[5]= 11.; yran[6]= 13.; yran[7]= 17.;
        yran[8]= 19.; yran[9]= 49.; yran[10]= 1.; yran[11]=31.;
        yran[12]=37.; yran[13]= 5.; yran[14]= 7.; yran[15]=47.;
        yran[16]=61.; yran[17]=17.; yran[18]= 5.; yran[19]=23.;
        yran[20]=29.; yran[21]= 3.; yran[22]= 5.; yran[23]=41.;
        yran[24]=43.; yran[25]= 9.; yran[26]=13.; yran[27]=59.;
        yran[28]= 1.; yran[29]=67.;

        zran[0]=1.0;   zran[1]= 1.5;  zran[2]= 1.7;  zran[3]= 1.4;
        zran[4]= 1.9;  zran[5]= 1.0;  zran[6]= 1.5;  zran[7]= 1.2;
        zran[8]= 1.8;  zran[9]= 1.4;  zran[10]= 1.8; zran[11]= 1.7;
        zran[12]= 1.9; zran[13]= 1.5; zran[14]= 1.2; zran[15]= 1.1;
        zran[16]= 1.3; zran[17]= 1.7; zran[18]= 1.2; zran[19]= 1.6;
        zran[20]=1.9;  zran[21]= 1.0; zran[22]= 1.6; zran[23]= 1.3;
        zran[24]= 1.4; zran[25]= 1.8; zran[26]= 1.7; zran[27]= 1.5;
        zran[28]= 1.1; zran[29]= 1.0;
/*
** Set the min and max data values.
*/
        xmin = 0.0;
        xmax = 65.0;
        ymin =  0.0;
        ymax = 68.0;
/*
** Choose the X and Y coordinates for interpolation points on the
** regular grid.
*/
        for(i=0; i<MREG; ++i)
        {
                xreg[i] = xmin + (xmax - xmin) * (float)(i)/MREG;
        }

        for(i=0; i<NREG; ++i)
        {
                yreg[i] = ymin + (ymax - ymin) * (float)(i)/NREG;
        }

/* Interpolate data onto a regular grid */
        c_idsfft(1,NRAN,xran,yran,zran,MREG,NREG,MREG,xreg,yreg,zreg,iwrk,rwrk);
}

