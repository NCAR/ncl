/*
 *  $Id: c_stex01.c.sed,v 1.3 1994-08-02 16:52:41 haley Exp $
 */
#include <stdio.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define M 20
#define N 36

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
    float a[N][M], b[N][M], wrk[2*M*N], *p, rval;
    int i, j, *idm, lp, ival;
    extern int stumsl_();
	extern void dfclrs();
/*
 *     open gks, open workstation, activate workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws(WKID, NULL, WSTYPE);
    gactivate_ws(WKID);

/*
 * Define a GKS color table
 */
	dfclrs();
/*
 * Do the set call, set the mapping mode and data coordinate
 * boundaries appropriately for polar coordinate mapping
 */
    c_set (0.05,0.95,0.05,0.95,-20.0,20.0,-20.0,20.0,1);
    c_stseti("MAP -- mapping mode", 2);
    c_stseti("SET -- do set call", 0);
    c_stsetr("XC1 -- lower x bound", 1.0);
    c_stsetr("XCM -- upper x bound", 20.0);
    c_stsetr("YC1 -- lower y bound", 0.0);
    c_stsetr("YCN -- upper y bound", 360.0);
    for( i = 0; i < N; i++ ) {
        for( j = 0; j < M; j++ ) {
            a[i][j] = 1.0;
            b[i][j] = 0.;
        }
    }

    c_stinit((float *)a,M,(float *)b,M,p,lp,M,N,wrk,2*M*N);
    gset_line_colr_ind(7);
    c_stream((float *)a,(float *)b,p,idm,stumsl_,wrk);
    gset_line_colr_ind(2);
    for( i = 0; i < N; i++ ) {
        for( j = 0; j < M; j++ ) {
            a[i][j] = 0.;
            b[i][j] = -1.;
        }
    }
    c_stinit((float *)a,M,(float *)b,M,p,lp,M,N,wrk,2*M*N);
    c_stream((float *)a,(float *)b,p,idm,stumsl_,wrk);
    c_frame();
/*
 *     deactivate and close workstation, close gks.
 */
/*
 * Test c_stgeti
 */
    c_stseti("MAP -- mapping mode", 2);
    c_stgeti("MAP -- mapping mode", &ival);
	if( ival != 2 ) {
		fprintf( stderr, "c_stgeti test UNSUCCESSFUL\n" );
		fprintf( stderr, "ival should be 2, ival is really %d\n", ival );
	}
/*
 * Test c_stgetr
 */
    c_stsetr("YCN -- upper y bound", 85.0);
    c_stgetr("YCN -- upper y bound", &rval);
	if( rval != 85. ) {
		fprintf( stderr, "c_stgetr test UNSUCCESSFUL\n" );
	    fprintf( stderr, "rval should be 85.0, rval is really %g\n", rval );
	}
/*
 * Close and deactivate workstation
 */
    gdeactivate_ws (WKID);
    gclose_ws(WKID);
/*
 * Close GKS
 */
    gclose_gks();
}

#define  NCLRS   16

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
	rgb[0].rgb.red = 0.00 ; rgb[0].rgb.green = 0.00 ; rgb[0].rgb.blue = 0.00 ;
	rgb[1].rgb.red = 1.00 ; rgb[1].rgb.green = 1.00 ; rgb[1].rgb.blue = 1.00 ;
	rgb[2].rgb.red = 0.70 ; rgb[2].rgb.green = 0.70 ; rgb[2].rgb.blue = 0.70 ;
	rgb[3].rgb.red = 0.75 ; rgb[3].rgb.green = 0.50 ; rgb[3].rgb.blue = 1.00 ;
	rgb[4].rgb.red = 1.00 ; rgb[4].rgb.green = 0.00 ; rgb[4].rgb.blue = 1.00 ;
	rgb[5].rgb.red = 0.00 ; rgb[5].rgb.green = 0.00 ; rgb[5].rgb.blue = 1.00 ;
	rgb[6].rgb.red = 0.00 ; rgb[6].rgb.green = 0.50 ; rgb[6].rgb.blue = 1.00 ;
	rgb[7].rgb.red = 0.00 ; rgb[7].rgb.green = 1.00 ; rgb[7].rgb.blue = 1.00 ;
	rgb[8].rgb.red = 0.00 ; rgb[8].rgb.green = 1.00 ; rgb[8].rgb.blue = 0.60 ;
	rgb[9].rgb.red = 0.00 ; rgb[9].rgb.green = 1.00 ; rgb[9].rgb.blue = 0.00 ;
	rgb[10].rgb.red = 0.70; rgb[10].rgb.green = 1.00; rgb[10].rgb.blue = 0.00 ;
	rgb[11].rgb.red = 1.00; rgb[11].rgb.green = 1.00; rgb[11].rgb.blue = 0.00 ;
	rgb[12].rgb.red = 1.00; rgb[12].rgb.green = 0.75; rgb[12].rgb.blue = 0.00 ;
	rgb[13].rgb.red = 1.00; rgb[13].rgb.green = 0.38; rgb[13].rgb.blue = 0.38 ;
	rgb[14].rgb.red = 1.00; rgb[14].rgb.green = 0.00; rgb[14].rgb.blue = 0.38 ;
	rgb[15].rgb.red = 1.00; rgb[15].rgb.green = 0.00; rgb[15].rgb.blue = 0.00 ;
/*
 * Define 16 different color indices, for indices 0 through 15.  The
 * color corresponding to index 0 is black and the color corresponding
 * to index 1 is white.
 */
	for( i = 0; i < NCLRS; i++ ) {
	    gset_colr_rep(WKID, i, &rgb[i] );
	}
    return;
}

