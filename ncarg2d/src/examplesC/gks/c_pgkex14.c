#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define MAX(x,y)  ((x) > (y) ? (x) : (y))

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
/*
 *  Illustrate character height and up vector.
 */
    Gcolr_rep       rgbs[3];
    Gdouble         rtchh;
    Gtext_font_prec tfp;
    Gtext_align     text_align;
    Gvec            vector;
    Gpoint          pos;
    float dtr = .017453292519943;
    int i,iang,ierr;
    float xoff,yoff,ang;
    float xcd,ycd;
    float chux,chuy,chh;
    float cs,sn;
/*
 *
 *  Open GKS, open and activate the metafile workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws( WKID, NULL, WSTYPE);
    gactivate_ws( WKID );
/*
 *  Define necessary color indices.
 */
    rgbs[0].rgb.red = 1.; rgbs[0].rgb.green = 1.; rgbs[0].rgb.blue = 1.;
    rgbs[1].rgb.red = 0.; rgbs[1].rgb.green = 0.; rgbs[1].rgb.blue = 1.;
    rgbs[2].rgb.red = .4; rgbs[2].rgb.green = 0.; rgbs[2].rgb.blue = .4;
    for( i = 0; i <= 2; i++ ) {
        gset_colr_rep(WKID,i,&rgbs[i]);
    }
    tfp.font = -4;
    tfp.prec = GPREC_STROKE;
    gset_text_font_prec(&tfp);
/*
 *  Alignment = [center, center]
 */
    text_align.hor = GHOR_CTR;
    text_align.vert = GVERT_HALF;
    gset_text_align(&text_align);
/*
 *
 *  in increments proportional to character heights.  Position text 
 *  strings centered on a circular arc with up vectors being tangents 
 *  to the arc.
 */
    iang = 1;
m110:
    ang=dtr*(iang);
    xoff = .3;
    yoff = .2;
/*
 *  Calculate an (X,Y) coordinate on the arc.
 */
    cs = (float) cos( ang);
    sn = (float) sin( ang);
    xcd=xoff+.4*cs;
    ycd=yoff+.4*sn;
/*
 *  The up vector is tangent to the circular arc.
 */ 
    chux = vector.delta_x = -(ycd-yoff);
    chuy = vector.delta_y =   xcd-xoff;
    gset_char_up_vec(&vector);
/*
 *  Scale the character heights depending on the angle and plot the text.
 */
    chh = .0004*(136-iang);
    gset_char_ht((Gdouble)chh);
    pos.x = xcd;
    pos.y = ycd;
    gtext(&pos,"NCAR");
/*
 *  Increment the angle by an amount proportional to the character heights.
 */
    iang = ((float)(iang))+MAX(210.*chh,1.);
    if (iang < 135) goto m110;
    ginq_char_ht(&ierr,&rtchh);
    if (rtchh != chh) {
        printf("GINQ_CHAR_HT test UNSUCCESSFUL\n");
    }
    else {
        printf("GINQ_CHAR_HT test SUCCESSFUL\n");
    }
    ginq_char_up_vec(&ierr,&vector);
    if (vector.delta_x != chux || vector.delta_y != chuy) {
        printf("GINQ_CHAR_UP_VEC test UNSUCCESSFUL\n");
    }
/*
 *  Plot a character string with the up vector being down.
 */
    vector.delta_x = 0.;
    vector.delta_y = -1.;
    gset_char_up_vec(&vector);
    gset_char_ht(.03);
    pos.x = 0.25;
    pos.y = 0.34;
    gtext(&pos,"NCAR");
    vector.delta_y = 1.;
    gset_char_up_vec(&vector);
    gset_text_colr_ind(2);
    gset_char_ht(.025);
    pos.x = 0.25;
    pos.y = 0.40;
    gtext(&pos,"Vect.=(0.,-1.)");
/*
 *  Plot a character string with up vector at an angle to the right.
 */
    vector.delta_x = 1.6;
    vector.delta_y = 2.;
    gset_char_up_vec(&vector);
    gset_char_ht(.03);
    gset_text_colr_ind(1);
    pos.x = 0.65;
    pos.y = 0.65;
    gtext(&pos,"NCAR");
    vector.delta_x = 0.;
    vector.delta_y = 1.;
    gset_char_up_vec(&vector);
    gset_text_colr_ind(2);
    gset_char_ht(.025);
    pos.x = 0.8;
    pos.y = 0.7;
    gtext(&pos,"Vect.=(1.6,2.)");
/*
 *  Label the plot (PLOTCHAR strokes its characters with lines, so the
 *  PLOTCHAR character attributes are controlled by the GKS polyline 
 *  attributes.
 */
    c_pcseti("FN",25);
    c_pcseti("CC",2);
    c_plchhq(.1,.89,"Character heights &",.035,0.,-1.);
    c_plchhq(.1,.82,"Character up vectors",.035,0.,-1.);
    c_frame();
/*
 *  Deactivate and close the workstation, close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}
