#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
/*
 *  Illustrate character spacings and expansion factors.
 */
    Gint idum;
    Gcolr_rep       rgbs[3];
    Gtext_align     text_align;
    Gtext_font_prec tfp;
    Gpoint          pos;
    float xp[3],setp;
    Gdouble rtsetp;
    float y,cexp;
    Gdouble rtspacng, spacng;
    char label[20];
    int i,j,hold[3];
    int ierr;
    
    xp[1]= .5;
    xp[2]= 2.;
    hold[0] = 0;
    hold[1] = 2;
    hold[2] = 1;
/*
 *  Open GKS, open and activate the metafile workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws( WKID, NULL, WSTYPE);
    gactivate_ws( WKID );
/*
 *  Define necessary color indices.
 */
    rgbs[0].rgb.red = 0.;
    rgbs[0].rgb.green = 0.;
    rgbs[0].rgb.blue = 0.;
    rgbs[1].rgb.red = 1.;
    rgbs[1].rgb.green = 1.;
    rgbs[1].rgb.blue = 1.;
    rgbs[2].rgb.red = 1.;
    rgbs[2].rgb.green = 1.;
    rgbs[2].rgb.blue = 0.;
    for( i = 0; i <= 2; i++ ) {
        gset_colr_rep(WKID,i,&rgbs[i]);
    }
/*
 *  Alignment = [center, center]
 */
    text_align.hor = GHOR_CTR;
    text_align.vert = GVERT_HALF;
    gset_text_align(&text_align);
/*
 *  Character spacings.
 */
    y = .95;
    
    for(j=0;j<3;j++){
        i = hold[j];
        spacng = 0.5*i;
        sprintf(label," Spacing = %4.1f",spacng);
    /*
     *  Attributes for the label for the demo text string.
     */
        tfp.font = -12;
        tfp.prec = GPREC_STROKE;
        gset_text_font_prec(&tfp);
        gset_char_ht(.033);
        gset_text_colr_ind(2);
        gset_char_space(0.);
        pos.x = .5;
        pos.y = y;
        gtext(&pos,label);
        y = y-.08;
    /*
     *  Attributes for the demo string.
     */
        gset_char_ht(.04);
        gset_char_space(spacng);
        tfp.font = -13;
        tfp.prec = GPREC_STROKE;
        gset_text_font_prec(&tfp);
        gset_text_colr_ind(1);
        pos.x = .5;
        pos.y = y;
        gtext(&pos,"NCAR Graphics");
        y = y-.12;
    }
    ginq_char_space(&ierr,&rtspacng);
    if(rtspacng != spacng) {
        printf("GINQ_CHAR_SPACE test UNSUCCESSFUL\n");
        printf( "rtspacng = %g  spacng = %g\n", rtspacng, spacng );
    }
    else {
        printf("GINQ_CHAR_SPACE test SUCCESSFUL\n");
    }
/*
 *  Character expansion factors.
 */
    for(i=1;i<3;i++){
        cexp = xp[i];
        sprintf(label,"Expansion = %4.1f",cexp);
    /*
     *  Attributes for the label for the demo text string.
     */
        tfp.font = -12;
        tfp.prec = GPREC_STROKE;
        gset_text_font_prec(&tfp);
        gset_char_ht(.033);
        gset_text_colr_ind(2);
        gset_char_space(0.);
        setp = 1.;
        gset_char_expan((Gdouble)setp);
        pos.x = .5;
        pos.y = y;
        gtext(&pos,label);
        y = y-.08;
    /*
     *  Attributes for the demo string.
     */
        gset_char_ht(.04);
        gset_char_expan((Gdouble)cexp);
        tfp.font = -13;
        tfp.prec = GPREC_STROKE;
        gset_text_font_prec(&tfp);
        gset_text_colr_ind(1);
        pos.x = .5;
        pos.y = y;
        gtext(&pos,"NCAR Graphics");
        y = y-.12;
    }
    ginq_char_expan(&ierr,&rtsetp);
    if(rtsetp != cexp){
        printf("GINQ_CHAR_EXPAN test UNSUCCESSFUL\n");
        printf( "rtsetp = %g  cexp = %g\n", rtsetp, cexp );
    }
    else {
        printf("GINQ_CHAR_EXPAN test SUCCESSFUL\n");
    }
    c_frame();
/*
 *  Deactivate and close the workstation, close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}
