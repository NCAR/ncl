/*
 *	$Id: c_example.c,v 1.1 1994-05-13 14:18:18 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

main()
{
    float y[21];
    int idum, i;
/*
 * OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
 */
    gopen_gks ("6",0);
    gopen_ws (1, NULL, 1);
    gactivate_ws (1);
/*
 * Fill Y array for entry EZY of AUTOGRAPH.
 */
    for( i = 0; i < 21; i++ ) {
        y[i] = exp(-.1*(float)(i+1))*cos((float)(i+1)*.5);
    }

    c_ezy (y,21,"DEMONSTRATING EZY ENTRY OF AUTOGRAPH$");
/*
 *     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
    gdeactivate_ws(1);
    gclose_ws(1);
    gclose_gks();
}
