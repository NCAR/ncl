/*
 * $Id: c_cardb2.c,v 1.2 1994-06-21 14:53:53 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define LMAP   150000
#define NMAP   43
#define NPTS   50
#define CNTR   5

#define WSTYPE SED_WSTYPE
#define WKID   1

float xgeo[] = {.63, .12, .05, .07, .10, .04, .19, .31, .31, .41, .39, .47, 
                .64, .63, .70, .66, .67, .69, .76, .92, .95, .69, .64, .53,
                .53, .60, .63, .63, .72, .74, .79, .75, .75, .80, .75, .70, 
                .68, .64, .63, .55,.55, .63, .63};

float ygeo[] = { .94, .95, .92, .85, .83, .78, .84, .75, .69, .58, .64, .55, 
                 .47, .37, .30, .05, .03, .05, .13, .26, .38, .52, .50, .57, 
                 .63, .63, .59, .64, .72, .71, .75, .75, .77, .78, .85, .83, 
                 .86, .86, .77, .80, .86, .90, .94};

float x[] = {.10, .22, .25, .25, .25, .50, .30, .47, .50, .77, .75, .68};
float y[] = {.98, .70, .55, .38, .18, .18, .90, .85, .70, .35, .18, .05};

float xperim[] = {0.0, 1.0, 1.0, 0.0, 0.0};
float yperim[] = {0.0, 0.0, 1.0, 1.0, 0.0};

main()
{
    int j, map[LMAP];
    float xcntr[NPTS], ycntr1[NPTS], ycntr2[NPTS], ycntr3[NPTS];
    float ycntr4[NPTS], ycntr5[NPTS], dist;

    xcntr[0] = 0.0;
    ycntr1[0] = 0.27;
    ycntr2[0] = 0.22;
    ycntr3[0] = 0.60;
    ycntr4[0] = 0.80;
    ycntr5[0] = 0.95;
    for( j = 1; j < NPTS; j++ ) {
        dist = (float)(j+1)/(float)NPTS;
        xcntr[j] = dist;
        ycntr1[j] = .1*cos((float)(4*3.14*dist))+.15;
        ycntr2[j] = .1*sin((float)(4*3.14*dist-.78))+.30;
        ycntr3[j] = .1*cos((float)(4*3.14*dist))+.50;
        ycntr4[j] = .1*cos((float)(4*3.14*dist))+.70;
        ycntr5[j] = .1*cos((float)(4*3.14*dist))+.85;
    }
/*
 * Open GKS
 */
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, WSTYPE);
    gactivate_ws(WKID);
/*
 * Outline continents in red
 */
    c_curve (xgeo,ygeo,NMAP);
/*
 * Outline contours and perimeter in green
 */
    c_curve (xcntr,ycntr1,NPTS);
    c_curve (xcntr,ycntr2,NPTS);
    c_curve (xcntr,ycntr3,NPTS);
    c_curve (xcntr,ycntr4,NPTS);
    c_curve (xcntr,ycntr5,NPTS);
    c_curve (xperim,yperim,5);
/*
 * Initialize Areas
 */
    c_arinam (map,LMAP);
/*
 * Add continents to area map in group 1.
 */
    c_aredam (map, xgeo, ygeo, NMAP, 1, 2, 1);
/*
 * Add contours and perimeter to area map in group 3.
 */
    c_aredam (map, xcntr, ycntr1, NPTS, 3, 2, 1);
    c_aredam (map, xcntr, ycntr2, NPTS, 3, 3, 2);
    c_aredam (map, xcntr, ycntr3, NPTS, 3, 4, 3);
    c_aredam (map, xcntr, ycntr4, NPTS, 3, 5, 4);
    c_aredam (map, xcntr, ycntr5, NPTS, 3, 6, 5);
/*
 * Run debugging subroutine on group 3
 */
    c_ardbpa (map, 3, "Crossing Contours");
    c_ardbpa (map, 1, "Crossing Contours");
/*
 * Close GKS
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}
