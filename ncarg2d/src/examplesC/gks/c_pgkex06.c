#include <math.h>
#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define ILD 121

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
/*
 *  Illustrate polylines with different colors, widths, and types.
 */
    Gdouble         rwd;
    Gcolr_rep       rgbs[6];
    Gpoint_list     line;
    Gpoint          pos;
    Gint colr_ind;
    Gtext_align     text_align;
    Gtext_font_prec tfp;
    char label1[9], label2[12];
    float scale;
    int rln,ln;
    float x,y,wdth;
    float plx[ILD],ply[ILD];
    int i,j,k,ierr;
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
    rgbs[0].rgb.red = 1.;    rgbs[0].rgb.green = 1.;    rgbs[0].rgb.blue = 1.;
    rgbs[1].rgb.red = 0.;    rgbs[1].rgb.green = 0.;    rgbs[1].rgb.blue = 0.;
    rgbs[2].rgb.red = 1.;    rgbs[2].rgb.green = 0.;    rgbs[2].rgb.blue = 0.;
    rgbs[3].rgb.red = 0.;    rgbs[3].rgb.green = 0.;    rgbs[3].rgb.blue = 1.;
    rgbs[4].rgb.red = 0.;    rgbs[4].rgb.green = 1.;    rgbs[4].rgb.blue = 0.;
    rgbs[5].rgb.red = .4;   rgbs[5].rgb.green = 0.;    rgbs[5].rgb.blue = 0.4;
    for( i = 0; i <= 5; i++ ) {
        gset_colr_rep(WKID,i,&rgbs[i]);
    }
/*
 *  Generate data for a spiral.
 */
    line.num_points = ILD;
    line.points = (Gpoint *)malloc(line.num_points*sizeof(Gpoint));
    if( !line.points ) {
		fprintf( stderr, "c_pgkex06: Not enough memory to create polyline structure\n" );
		gemergency_close_gks();
		exit(1);
	}
    j = 0;
    for( i = 0; i <= 720; i+=6 ) {
        scale = (float) i/4500.;
        plx[j] = scale * (float) cos( (double)(i-1)*radc);
        ply[j] = scale * (float) sin( (double)(i-1)*radc);
        j++;
    }
/*
 *  Draw spirals with different linetypes, colors, and widths.
 */
    for(i = 1; i <= 4; i++){
        x = .28+.42*(float)(i % 2);
        y = .25+.41*(float)((int)((i-1)/2));
        gset_line_colr_ind(i);
        wdth = 2.*(i-1)+1.;
        gset_linewidth((Gdouble)wdth);
        gset_linetype(i);
        for( j = 0; j < ILD; j++ ){
            line.points[j].x = x+plx[j];
            line.points[j].y = y+ply[j];
        }
        gpolyline(&line);
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
        pos.y = y+.18;
        gtext(&pos,label1);
        pos.y = y+.14;
        gtext(&pos,label2);
    }
    ginq_linewidth(&ierr,&rwd);
    if((ierr != 0) || (rwd != wdth)) {
        printf("GINQ_LINEWIDTH test UNSUCCESSFUL\n");
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
    c_pcseti("FN",25);
    c_pcseti("CC",5);
    c_plchhq(.5,.93,"Polylines",.035,0.,0.);
    c_frame();
/*
 * Test ginq_text_align
 */
    text_align.hor = GHOR_LEFT;
    text_align.vert = GVERT_HALF;
    gset_text_align(&text_align);
    ginq_text_align(&ierr,&text_align);
    if( text_align.hor != GHOR_LEFT || text_align.vert != GVERT_HALF ) {
        printf("GINQ_TEXT_ALIGN test UNSUCCESSFUL\n" );
    }
/*
 * Test ginq_text_font_prec
 */
    tfp.font = -12;
    tfp.prec = GPREC_STROKE;
    gset_text_font_prec(&tfp);
    ginq_text_font_prec(&ierr,&tfp);
    if( tfp.font != -12 || tfp.prec != GPREC_STROKE ) {
        printf("GINQ_TEXT_FONT_PREC test UNSUCCESSFUL\n" );
    }
/*
 * Test ginq_text_colr_ind
 */
    gset_text_colr_ind(5);
    ginq_text_colr_ind(&ierr,&colr_ind);
    if( colr_ind != 5 ) {
        printf("GINQ_TEXT_COLR_IND test UNSUCCESSFUL\n" );
    }
/*
 * Free memory
 */
    free(line.points);
/*
 *  Deactivate and close the workstation, close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}
