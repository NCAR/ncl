#include <math.h>
#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
    
/*
 *  Illustrate polylines with different colors, widths, and types.
 */
    Gint            idum;
    Gdouble         rwd;
    Gcolr_rep       rgbs[6];
    Gpoint_list     point_list;
    Gpoint          pos;
    Gtext_align     text_align;
    Gtext_font_prec tfp;
    char label1[8], label2[11];
    int ild = 121;
    float scale;
    float cs,sn;
    int rln,ln;
    float x,y,wdth;
    int i,j,k,ierr;
    float  plx[121],ply[121];
    float radc = .0174532;
/*
 *  Open GKS, open and activate the metafile workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws( WKID, NULL, WSTYPE);
    gactivate_ws( WKID );
/*
 *  Define the necessary color indices, color index 0 defines the 
 *  background color.
 */
    rgbs[0].rgb.red = 0.;
    rgbs[0].rgb.green = 0.;
    rgbs[0].rgb.blue = .6;
    rgbs[1].rgb.red = 1.;
    rgbs[1].rgb.green = 1.;
    rgbs[1].rgb.blue = 1.;
    rgbs[2].rgb.red = 1.;
    rgbs[2].rgb.green = 0.;
    rgbs[2].rgb.blue = 0.;
    rgbs[3].rgb.red = 0.;
    rgbs[3].rgb.green = 1.;
    rgbs[3].rgb.blue = 0.;
    rgbs[4].rgb.red = 1.;
    rgbs[4].rgb.green = 1.;
    rgbs[4].rgb.blue = 0.;
    rgbs[5].rgb.red = .65;
    rgbs[5].rgb.green = 1.;
    rgbs[5].rgb.blue = 1.;
    for( i = 0; i <= 5; i++ ) {
        gset_colr_rep(WKID,i,&rgbs[i]);
    }
/*
 *  Generate data for a spiral.
 */
    j = 0;
    for( i = 0; i < 721; i+=6 ) {
        scale = (float) i/3500;
        j = j+1;
        cs = (float) cos( (double)(i-1)*radc);
        sn = (float) sin( (double)(i-1)*radc);
        plx[j-1] =  scale*cs;
        ply[j-1] =  scale*sn;
    }
/*
 *  Draw spirals with different linetypes, colors, and widths.
 */
    for(i=1;i<5;i++){
        x = .28+.42*(i % 2);
        y = .25+.45*((int)((i-1)/2));
        gset_line_colr_ind(i);
        wdth = 2.*(i-1)+1;
        gset_linewidth((Gdouble)wdth);
        gset_linetype(i);
        point_list.num_points = ild;
        point_list.points = (Gpoint *)calloc(ild,sizeof(Gpoint));
        for( j = 0; j < ild; j++ ){
            point_list.points[j].x = x+plx[j];
            point_list.points[j].y = y+ply[j];
        }
        gpolyline(&point_list);
    /*
     *  Label the lines.
     */
        sprintf(label1,"Type = %1d",i);
        sprintf(label2,"Width = %3.1f",wdth);
        text_align.hor = GHOR_CTR;
        text_align.vert = GVERT_HALF;
        gset_text_align(&text_align);
        gset_char_ht(.022);
        tfp.font = -12;
        tfp.prec = GPREC_STROKE;
        gset_text_font_prec(&tfp);
        gset_text_colr_ind(5);
        pos.x = x;
        pos.y = y+.22;
        gtext(&pos,label1);
        pos.y = y+.17;
        gtext(&pos,label2);
    }
    ginq_linewidth(&ierr,&rwd);
    if((ierr != 0) || (rwd != wdth)) {
        printf("GINQ_LINEWIDTH test UNSUCCESSFUL\n");
    }
    else {
        printf("GINQ_LINEWIDTH test SUCCESSFUL\n");
    }
/*    
 *  Label the plot (PLOTCHAR draws lines to plot characters,
 *  so the text color is controlled by the GKS polyline color).
 */
    gset_line_colr_ind(4);
    ln = 1;
    gset_linetype(ln);
    gset_linewidth(2.);
    ginq_linetype(&ierr,&rln);
    if( ierr !=0 || rln != ln ) {
        printf("GINQ_LINETYPE test UNSUCCESSFUL\n" );
    }
    else {
        printf("GINQ_LINETYPE test SUCCESSFUL\n" );
    }
    c_pcseti("CD",1);
    c_plchhq(.5,.95,"Polylines",.025,0.,0.);
    c_frame();
/*
 *  Deactivate and close the workstation, close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}
