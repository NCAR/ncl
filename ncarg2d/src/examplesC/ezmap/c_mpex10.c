/*
 *	$Id: c_mpex10.c,v 1.2 1994-06-21 15:00:15 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

/*
 * NCLS specifies the number of cells along each edge of the cell array.
 * Use a positive value less than or equal to 300.
 */
#define NCLS 300

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
    char proj[3],jlts[3];
    float plm1[2],plm2[2],plm3[2],plm4[2];
    float plat, plon, rota, salt, alfa, beta, dtor;
    float x, y, rlat, rlon, dval;
    int i, j, l, nclr, nclr1, igrd, iclr, ii, imin;
    Grect rect;
    Gpat_rep icra;
    Gcolr_rep rgb;
    extern void bndary();
/*
 * NCLR specifies the number of different colors to be used.
 */
    nclr = 64;
    nclr1 = 65;
/*
 * PROJ is the desired projection type.  Use one of 'LC', 'ST', 'OR',
 * 'LE', 'GN', 'AE', 'SV', 'CE', 'ME', or 'MO'.
 */
    strcpy( proj, "OR" );
/*
 * PLAT and PLON are the desired latitude and longitude of the center of
 * the projection, in degrees.
 */
    plat = 40.;
    plon = -105.;
/*
 * ROTA is the desired final rotation of the map, in degrees.
 */
    rota = 0.;
/*
 * SALT, ALFA, and BETA are the desired values of the parameters 'SA',
 * 'S1', and 'S2', which are only used with a satellite-view projection.
 * SALT is the distance of the satellite from the center of the earth,
 * in units of earth radii.  ALFA is the angle, in degrees, between the
 * line of sight and the line to the center of the earth.  BETA is used
 * only when ALFA is non-zero; it is the angle, in degrees, measured
 * counterclockwise, from the plane passing through the satellite, the
 * center of the earth, and the point which is due east on the horizon
 * to the plane in which the line of sight and the line to the center
 * of the earth both lie.
 */
    salt = 1.25;
    alfa = 15.;
    beta = 90.;
/*
 * JLTS, PLM1, PLM2, PLM3, and PLM4 are the required arguments of the
 * EZMAP routine MAPSET, which determines the boundaries of the map.
 */
    strcpy( jlts, "MA" );
    plm1[0] = plm2[0] = plm3[0] = plm4[0] = 0;
    plm1[1] = plm2[1] = plm3[1] = plm4[1] = 0;
/*
 * IGRD is the spacing, in degrees, of the EZMAP grid of latitudes and
 * longitudes.
 */
    igrd = 15;
/*
 * Define the constant used to convert from degrees to radians.
 */
    dtor = .017453292519943;
/*
 * Open GKS.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Define the color indices required.  0 and 1 are used for black and
 * white (as is customary); the next NCLR values are distributed between
 * pure blue (color 2) and pure red (color NCLR+1).
 */
    rgb.rgb.red = rgb.rgb.green = rgb.rgb.blue = 0.;
    gset_colr_rep (WKID,0,&rgb);
    rgb.rgb.red = rgb.rgb.green = rgb.rgb.blue = 1.;
    gset_colr_rep (WKID,1,&rgb);

    rgb.rgb.green = 0.;
    for( iclr=1; iclr <= nclr; iclr++ ) {
        rgb.rgb.red = (float)(iclr-1)/(float)(nclr-1);
        rgb.rgb.blue = (float)(nclr-iclr)/(float)(nclr-1);
        gset_colr_rep (WKID,1+iclr,&rgb);
    }
/*
 * Set the EZMAP projection parameters.
 */
    c_maproj (proj,plat,plon,rota);
    if (!strcmp(proj,"SV") ) {
        c_mpsetr ("SA",salt);
        c_mpsetr ("S1",alfa);
        c_mpsetr ("S2",beta);
    }
/*
 * Set the limits of the map.
 */
    c_mapset (jlts,plm1,plm2,plm3,plm4);
/*
 * Set the grid spacing.
 */
    c_mapsti ("GR - GRID SPACING",igrd);
/*
 * Initialize EZMAP, so that calls to MAPTRI will work properly.
 */
    c_mapint();
/*
 * Fill the cell array.  The data generator is rigged to create
 * values between 0 and 1, so as to make it easy to interpolate to
 * get a color index to be used.  Obviously, the statement setting
 * DVAL can be replaced by one that yields a value of some real data
 * field of interest (normalized to the range from 0 to 1).
 */
    icra.colr_array = (Gint *)malloc(NCLS*NCLS*sizeof(Gint));
    icra.dims.size_x = NCLS;
    icra.dims.size_y = NCLS;
    if( icra.colr_array ) {
        l = 0;
        for( i=1; i <= NCLS; i++ ) {
            x=c_cfux(.05+.90*((float)(i-1)+.5)/(float)(NCLS));
            for( j=1; j <= NCLS; j++ ) {
                y=c_cfuy(.05+.90*((float)(j-1)+.5)/(float)(NCLS));
                c_maptri (x,y,&rlat,&rlon);
                if (rlat <= 9.e11 ) {
                    dval=.25*(1.+cos(dtor*10.*rlat))+.25*(1.+sin(dtor*10.*rlon))*cos(dtor*rlat);
                    ii = 2+(int)(dval*(float)nclr);
                    imin = ii > nclr1 ? nclr1 : ii;
                    icra.colr_array[l++] = 2 > imin ? 2 : imin;
                }
                else {
                    icra.colr_array[l++] = 0;
                }
            }
        }
    }
    else {
        fprintf( stderr, "c_mpex10: no memory for cell array\n" );
        exit(1);
    }
/*
 * Draw the cell array.
 */
    rect.p.x = c_cfux(.05);
    rect.p.y = c_cfuy(.05);
    rect.q.x = c_cfux(.95);
    rect.q.y = c_cfuy(.95);
    gcell_array (&rect,&icra);
/*
 * Draw a map on top of the cell array.
 */
    c_mapdrw();
/*
 * Put a label at the top of the plot.
 */
    c_set (0.,1.,0.,1.,0.,1.,0.,1.,1);
    c_wtstr (.5,.975,"EXAMPLE 10",2,0,0);
/*
 * Draw a boundary around the edge of the plotter frame.
 */
    bndary();
/*
 * Advance the frame.
 */
    c_frame();
/*
 * Close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void bndary()
{
/*
 * Routine to draw the plotter-frame edge.
 */
    c_plotit(    0,    0,0);
    c_plotit(32767,    0,1);
    c_plotit(32767,32767,1);
    c_plotit(    0,32767,1);
    c_plotit(    0,    0,1);
}
