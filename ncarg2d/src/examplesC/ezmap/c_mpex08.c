/*
 *	$Id: c_mpex08.c,v 1.1 1994-10-31 03:29:13 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>

char *plbl = "ILLUSTRATING THE USE OF MAPUSR";

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{

/*
 * Produce a Mercator projection of the whole globe, using a
 * version of MAPUSR which dots the grid lines and dashes the
 * continental outlines.
 *
 * Define the label for the top of the map.
 */
    float plm1[2], plm2[2], plm3[2], plm4[2];
    int ierr;
    extern void bndary();
/*
 * Open GKS.
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Weird up the projection a little.
 */
    plm1[0] = plm2[0] = plm3[0] = plm4[0] = 0.;
    c_supmap (9,0.,0.,90.,plm1,plm2,plm3,plm4,1,15,2,0,&ierr);
/*
 * Put the label at the top of the plot.
 */
    c_set (0.,1.,0.,1.,0.,1.,0.,1.,1);
    c_pwrit (.5,.975,plbl,30,2,0,0);
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

NGCALLF(mapusr,MAPUSR)(iprt)
int *iprt;
{
/*
 * this version of mapusr forces the grid lines to be dotted and
 * the outlines to be dashed.
 */
/*
 * certain local parameters must be saved from call to call.
 */
    static int idtf,idbd;
    int ii;
/*
 * if iprt is positive, a part is about to be drawn.  save the
 * dotted/solid flag and/or the distance between dots and then
 * reset them and/or the dash pattern.
 */
    if (*iprt > 0) {
        if (*iprt == 2) {
            c_mapgti ("DL",&idtf);
            c_mapgti ("DD",&idbd);
            c_mapsti ("DL",1);
            c_mapsti ("DD",3);
        }
        else if (*iprt == 5) {
            c_mapgti ("DL",&idtf);
            c_mapsti ("DL",0);
            ii = 21845;
            c_dashdb (&ii);
        }
    }
    else {
/*
 * otherwise, a part has just been drawn.  restore saved settings
 * and/or select a solid dash pattern.
 */
        if (*iprt == -2) {
            c_mapsti ("DL",idtf);
            c_mapsti ("DD",idbd);
        }
        else if (*iprt == -5) {
            c_mapsti ("DL",idtf);
            ii = 65535;
            c_dashdb (&ii);
        }
    }
    return(1);
}
