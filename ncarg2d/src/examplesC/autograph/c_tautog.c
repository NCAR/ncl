/*
 *	$Id: c_tautog.c,v 1.1 1994-05-13 14:24:25 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

main()
{
    int idum, ierr;
/*
 * open gks, open workstation of type 1, activate workstation
 */
    gopen_gks ("stdout",0);
    gopen_ws (1, NULL, 1);
    gactivate_ws(1);
/*
 * invoke demo driver
 */
    tautog(&ierr);
/*
 *     deactivate and close workstation, close gks.
 */
    gdeactivate_ws(1);
    gclose_ws(1);
    gclose_gks();
}

tautog(ierror)
int *ierror;
{
    float x[21], y1d[21], y2d[5][21];
    int i, j;
    float t;
/*
 * fill y1d array for entry c_ezy.
 */
    for( i = 0; i < 21; i++ ) {
        y1d[i] = exp(-.1*(float)(i+1))*cos((float)(i+1)*.5);
    }
/*
 * entry c_ezy plots y1d as a function of a set of continuous integers.
 */
/*
 *       demonstrating c_ezy entry of autograph
 */
    c_ezy(y1d,21,"DEMONSTRATING EZY ENTRY OF AUTOGRAPH$");
/*
 *     frame 2 -- c_ezxy entry of autograph.
 */
/*
 * fill x and y1d arrays for entry c_ezxy.
 */
    for( i = 0; i < 21; i++ ) {
        x[i] = (float)(i)*.314;
        y1d[i] = x[i]+cos(x[i])*2.0;
    }
/*
 * set autograph control parameters for y-axis label   "x+cos(x)*2"
 */
    c_agsetc("LABEL/NAME.","L");
    c_agseti("LINE/NUMBER.",100);
    c_agsetc("LINE/TEXT.","X+COS(X)*2$");
/*
 * entry c_ezxy plots contents of x-array vs. y1d-array.
 */
/*
 *       demonstrating c_ezxy entry of autograph
 */
    c_ezxy(x,y1d,21,"DEMONSTRATING EZXY ENTRY IN AUTOGRAPH$");
/*
 *     frame 3 -- c_ezmy entry of autograph.
 */
/*
 * fill y2d array for entry c_ezmy.
 */
    for( i = 0; i < 21; i++ ) {
        t = .5*(float)(i);
        for( j = 0; j < 5; j++ ) {
            y2d[j][i] = exp(-.5*t)*cos(t)/(float)(j+1);
        }
    }
/*
 * set the autograph control parameters for y-axis label
 *         exp(-x/2)*cos(x)*scale
 */
    c_agsetc("LABEL/NAME.","L");
    c_agseti("LINE/NUMBER.",100);
    c_agsetc("LINE/TEXT.","EXP(-X/2)*COS(X)*SCALE$");
/*
 * use the autograph control parameter for integers to specify the
 * alphabetic set of dashed line patterns.
 */
    c_agseti("DASH/SELECTOR.",-1);
/*
 * use the autograph control parameter for integers to specify the
 * graph drawn is to be logarithmic in the x-axis.
 */
    c_agseti("X/LOGARITHMIC.",1);
/*
 * entry c_ezmy plots multiple arrays as a function of continuous integers.
 *
 *       demonstrating c_ezmy entry of autograph
 */
    c_ezmy(&y2d[0][0],21,5,10,"DEMONSTRATING EZMY ENTRY OF AUTOGRAPH$");
/*
 *     frame 4 -- c_ezmxy entry of autograph.
 *
 *
 * fill y2d array for c_ezmxy.
 */
    for( i = 0; i < 21; i++ ) {
        for( j = 0; j < 5; j++ ) {
            y2d[j][i] = pow(x[i],(float)(j+1))+cos(x[i]);
        }
    }
/*
 * set the autograph control parameters for y-axis label
 *         x**j+cos(x)
 */
    c_agsetc("LABEL/NAME.","L");
    c_agseti("LINE/NUMBER.",100);
    c_agsetc("LINE/TEXT.","X**J+COS(X)$");
/*
 * use the autograph control parameter for integers to specify the
 * alphabetic set of dashed line patterns.
 */
    c_agseti("DASH/SELECTOR.",-1);
/*
 * use the autograph control parameter for integers to specify the
 * graph have a linear x-axis and a logarithmic y-axis.
 */
    c_agseti("X/LOGARITHMIC.",0);
    c_agseti("Y/LOGARITHMIC.",1);
/*
 * entry c_ezmxy plots multiple y arrays as a function of a single
 * or multiple x arrays.
 *
 *       demonstrating c_ezmxy entry of autograph
 */
    for( i = 0; i < 21; i++ ) {
        for( j = 0; j < 5; j++ ) {
            y2d[j][i] = pow((double)x[i],(double)(j+1))+cos(x[i]);
        }
    }
    c_ezmxy(x,&y2d[0][0],21,5,21,"DEMONSTRATING EZMXY ENTRY OF AUTOGRAPH$");
/*
 * note that autograph makes its own frame advance calls.
 */
    *ierror = 0;
    printf("AUTOGRAPH TEST EXECUTED--SEE PLOTS TO CERTIFY\n");
    return(1);
}
