/*
 *  $Id: c_stex01.c.sed,v 1.1 1994-05-13 14:29:05 haley Exp $
 */
#include <stdio.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define M 20
#define N 36

main()
{
    float a[N][M], b[N][M], wrk[2*M*N], *p, rval;
    int i, j, *idm, lp, ival;
    extern int stumsl_();
/*
 *     open gks, open workstation, activate workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws(1, NULL, 1);
    gactivate_ws(1);

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
    printf( "c_stgeti:  ival should be 2, ival is really %d\n", ival );
/*
 * Test c_stgetr
 */
    c_stsetr("YCN -- upper y bound", 85.0);
    c_stgetr("YCN -- upper y bound", &rval);
    printf( "c_stgetr:  rval should be 85.0 rval is really %g\n", rval );
/*
 * Close and deactivate workstation
 */
    gdeactivate_ws (1);
    gclose_ws(1);
/*
 * Close GKS
 */
    gclose_gks();
}
