/*
 *	$Id: c_tagupw.c,v 1.1 1994-05-13 14:24:24 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

main()
{
    int idum,ierr;
/*
 * OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
 */
    gopen_gks ("stdout",0);
    gopen_ws (1, NULL, 1);
    gactivate_ws (1) ;
/*
 * INVOKE DEMO DRIVER
 */
    tagupw(&ierr);
/*
 *     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
    gdeactivate_ws (1);
    gclose_ws (1);
    gclose_gks();
}
tagupw (ierror)
int *ierror;
{
    int i, j;
    float t;
/*
 * Declare the data arrays X, Y1D, and Y2D.  X contains abscissae for
 * the plots produced by EZXY and EZMXY,  Y1D contains ordinates for
 * the plots produced by EZXY and EZY,  and Y2D contains ordinates for
 * the plots produced by EZMY and EZMXY.
 */
    float x[21],y1d[21],y2d[5][21];
/*
 * Fill the array Y1D for the call to EZY.
 */
    for( i = 0; i < 21; i++ ) {
        y1d[i] = exp(-.1*(float)(i+1))*cos((float)(i+1)*.5);
    }
/*
 * Plot the contents of Y1D as a function of the integers.
 */
    c_ezy (y1d,21,"DEMONSTRATING EZY ENTRY OF AUTOGRAPH$");
/*
 * Fill the arrays X and Y1D for the call to EZXY.
 */
    for( i = 0; i < 21; i++ ) {
        x[i] = (float)i*.314;
        y1d[i] = x[i]+cos(x[i])*2.0;
    }
/*
 * Redefine the Y-axis label.
 */
    c_agsetc("LABEL/NAME.","L");
    c_agseti("LINE/NUMBER.",100);
    c_agsetc("LINE/TEXT.","X+COS(X)*2$");
/*
 * Plot the array Y1D as a function of the array X.
 */
    c_ezxy (x,y1d,21,"DEMONSTRATING EZXY ENTRY IN AUTOGRAPH$");
/*
 * the array Y2D for the call to EZMY.;
 */
    for( i = 0; i < 21; i++ ) {
        t = .5*(float)i;
        for( j = 0; j < 5; j++ ) {
            y2d[j][i] = exp(-.5*t)*cos(t)/(float)(j+1);
        }
    }
/*
 * Redefine the Y-axis label.
 */
    c_agsetc("LABEL/NAME.","L");
    c_agseti("LINE/NUMBER.",100);
    c_agsetc("LINE/TEXT.","EXP(-X/2)*COS(X)*SCALE$");
/*
 * Specify that the alphabetic set of dashed line patterns is to be used.
 */
    c_agseti("DASH/SELECTOR.",-1);
/*
 * Specify that the graph drawn is to be logarithmic in X.
 */
    c_agseti("X/LOGARITHMIC.",1);
/*
 * Plot the five curves defined by Y2D as functions of the integers.
 */
    c_ezmy (&y2d[0][0],21,5,10,"DEMONSTRATING EZMY ENTRY OF AUTOGRAPH$");
/*
 * Fill the array Y2D for the call to EZMXY.
 */
    for( i = 0; i < 21; i++ ) {
        for( j = 0; j < 5; j++ ) {
            y2d[j][i] = pow((double)x[i],(double)(j+1))+cos(x[i]);
        }
    }
/*
 * Redefine the Y-axis label.
 */
    c_agsetc("LABEL/NAME.","L");
    c_agseti("LINE/NUMBER.",100);
    c_agsetc("LINE/TEXT.","X**J+COS(X)$");
/*
 * Specify that the graph is to be linear in X and logarithmic in Y.
 */
    c_agseti("X/LOGARITHMIC.",0);
    c_agseti("Y/LOGARITHMIC.",1);
/*
 * Plot the five curves defined by Y2D as functions of X.
 */
    c_ezmxy (x,&y2d[0][0],21,5,21,"DEMONSTRATING EZMXY ENTRY OF AUTOGRAPH$");
/*
 * Done.
 */
    *ierror = 0;
    printf( "AGUPWRTX TEST SUCCESSFUL\nSEE PLOTS TO VERIFY PERFORMANCE\n");
    return(1);
}
