#include <math.h>
#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
    Gcolr_rep rgb;
    int i;
    float x[100],y[100];
/*
 *  Open GKS, open and activate the metafile workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws( WKID, NULL, WSTYPE);
    gactivate_ws( WKID );
/*
 *  Define a small color table for the CGM workstation.
 */
    rgb.rgb.red = 1.0; rgb.rgb.green = 1.0; rgb.rgb.blue = 1.0;
    gset_colr_rep(WKID, 0, &rgb );
    rgb.rgb.red = 0.4; rgb.rgb.green = 0.0; rgb.rgb.blue = 0.4;
    gset_colr_rep(WKID, 1, &rgb );
    rgb.rgb.red = 0.0; rgb.rgb.green = 0.0; rgb.rgb.blue = 1.0;
    gset_colr_rep(WKID, 2, &rgb );
/*
 *  Turn clipping off
 */
    gset_clip_ind(GIND_NO_CLIP);
/*
 *  Generate a straight line of 100 points.
 */
    for( i = 1; i <= 100; i++ ) {
        x[i-1] = i;
        y[i-1] = 10.*i;
    }
/*
 *  Use SET to define normalization transformation 1 with linear
 *  scaling in the X direction and log scaling in the Y direction.
 */
    c_set(.15,.85,.15,.85,1.,100.,10.,1000.,2);
/*
 *  Set line color to blue.
 */
    gset_line_colr_ind(2);
/*
 *  Initialize the AUTOGRAPH entry EZXY so that the frame is not 
 *  advanced and the Y axis is logarithmic.
 */
    c_displa(2,0,2);
/*
 *  Output the polyline (X,Y) using EZXY.
 */
    c_ezxy(x,y,100," ");
/*
 *  Establish the identity transformation for character plotting.
 */
    c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
/*
 *  Title the plot using Plotchar.
 */
    c_pcseti("FN",25);
    c_pcseti("CC",2);
    c_plchhq(.5,.05,"Log Scaling with SPPS",.025,0.,0.);

    c_frame();
/*
 *  Deactivate and close the workstation, close GKS.
 */
    gdeactivate_ws (WKID);
    gclose_ws (WKID);
    gclose_gks();
}

