#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

main()
{
/*
 *  Illustrate filled area by replicating a basic filled area over a 
 *  large area to produce a pattern with perceptual multistability.
 */
    int i, j;
    Gcolr_rep rgb[5];
/*
 *  Open GKS, open and activate the metafile workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws(1, NULL, 8);
    gactivate_ws (1);
/*
 *     Define the color indices.
 */
    rgb[0].rgb.red = 0.;
    rgb[0].rgb.green = 1.;
    rgb[0].rgb.blue = 0.;
    rgb[1].rgb.red = 1.;
    rgb[1].rgb.green = 1.;
    rgb[1].rgb.blue = 1.;
    rgb[2].rgb.red = 0.;
    rgb[2].rgb.green = 0.;
    rgb[2].rgb.blue = 1.;
    rgb[3].rgb.red = 1.;
    rgb[3].rgb.green = 0.;
    rgb[3].rgb.blue = 0.;
    rgb[4].rgb.red = 0.;
    rgb[4].rgb.green = 0.;
    rgb[4].rgb.blue = 0.;
    for( i = 0; i < 5; i++ ) {
        gset_colr_rep(1,i,&rgb[i]);
    }
/*
 *  Replicate the small filled area over the entire plot.
 */
    for( j = 1; j <= 8; j+=2 ) {
        for( i = 1; i <= 10; i+= 2 ) {
            drwso(.05+(i-1)*.1,.14+(j-1)*.1,.1,0.);
            drwso(.15+(i-1)*.1,.14+(j-1)*.1,.1,180.);
            drwso(.05+(i-1)*.1,.24+(j-1)*.1,.1,180.);
            drwso(.15+(i-1)*.1,.24+(j-1)*.1,.1,0.);
        }
    }
/*
 *  Label the plot (PLOTCHAR strokes its characters with lines, so the
 *  PLOTCHAR character attributes are controlled by the GKS polyline 
 *  attributes.
 */
    gset_line_colr_ind(3);
    c_pcseti("CD",1);
    gset_linewidth(4.);
    c_plchhq(.5,.94,"Filled areas",.025,0.,0.);
/*
 *  Terminate the picture, deactivate and close the workstation, 
 *  close GKS.
 */
    c_frame();
    gdeactivate_ws (1);
    gclose_ws (1);
    gclose_gks();
}


#define ID  16
#define H   1.

float xa[ID] = {0.000000, 0.130602, 0.500000, 0.315301, 0.500000,  0.315301,
                0.130602, 0.130602, 0.000000,-0.130602,-0.130602, -0.315301,
               -0.500000,-0.315301,-0.500000,-0.130602};

float ya[ID+1] = {-0.500000, -0.369398, -0.369398, -0.184699, 0.000000,
                   0.184699,  0.000000,  0.369398,  0.500000, 0.369398,
                   0.000000,  0.184699,  0.000000, -0.184699,-0.369398,
                  -0.369398};

drwso(x,y,scale,angd)
float x, y, scale, angd;
{
/*
 *  Draw the fill area at coordinate (X,Y) at angle ANGD (in degrees)
 *  and scaled by SCALE.  Using a higher level of GKS one could use
 *  segment transformations to do the rotation, translation, and
 * scaling, but it is done directly here.
 */
    int k;
    float xb[ID+1],yb[ID+1];
    float angr, radc = .0174532;
    Gpoint_list fill_area;

    fill_area.num_points = ID;
    fill_area.points = (Gpoint *)malloc((ID+1)*sizeof(Gpoint));
/*
 *  Convert the angle to radians.
 */
    angr = radc*angd;
/*
 *  Translate, scale, and rotate the object so that its center is
 *  at (X,Y).
 */
    for( k = 0; k < ID; k++ ) {
        fill_area.points[k].x = x+scale*(xa[k]*cos(angr)-ya[k]*sin(angr));
        fill_area.points[k].y = y+scale*(xa[k]*sin(angr)+ya[k]*cos(angr));
    }
    gset_fill_int_style (GSTYLE_SOLID);
    gset_fill_colr_ind(2);
    fill_area.points[ID].x = fill_area.points[0].x;
    fill_area.points[ID].y = fill_area.points[0].y;
    gfill_area(&fill_area);
    free(fill_area.points);
}
