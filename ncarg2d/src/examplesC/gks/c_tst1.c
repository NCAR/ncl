#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define ID    16
#define IX    15
#define IMX   100
#define IMXH  IMX/2
#define IMXM  IMX-IX
#define IMXP  IMX+IX
#define IMXHM IMXH-IX
#define IMXHP IMXH+IX

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
/*
 *  Illustrate text alignment attributes.
 */
    Gcolr_rep       rgbs[4];
    Gcolr_rep       one;
    Gtext_align     text_align;
    Gtext_font_prec tfp;
    Gpoint          pos;
    float x,y;
    int i, ierr;
/*
 *  Open GKS, open and activate the metafile workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws( WKID, NULL, WSTYPE);
    gactivate_ws( WKID );
/*
 *  Define the necessary color indices.
 */
    rgbs[0].rgb.red = 0.;
    rgbs[0].rgb.green = 0.;
    rgbs[0].rgb.blue = .6;
    rgbs[1].rgb.red = 1.;
    rgbs[1].rgb.green = 1.;
    rgbs[1].rgb.blue = 1.;
    rgbs[2].rgb.red = 1.;
    rgbs[2].rgb.green = 1.;
    rgbs[2].rgb.blue = 0.;
    rgbs[3].rgb.red = 0.;
    rgbs[3].rgb.green = 1.;
    rgbs[3].rgb.blue = 0.;
    for( i = 0; i <= 3; i++ ) {
        gset_colr_rep(WKID,i,&rgbs[i]);
    }
    ginq_colr_rep( WKID, 2, 1, &ierr, &one );
    if( one.rgb.red != 1. || one.rgb.green != 1. || one.rgb.blue != 0. ) {
        printf( "GINQ_COLR_REP test UNSUCCESSFUL\n" );
    }
    else {
        printf( "GINQ_COLR_REP test SUCCESSFUL\n" );
    }
/*
 *  Specify the character height for all strings, select duplex 
 *  Roman font.
 */
    gset_char_ht(.025);
    tfp.font = -12;
    tfp.prec = GPREC_STROKE;
    gset_text_font_prec(&tfp);
    x = .50;
    y = .90;
/*
 *  Alignment = (1,3) [  left, center]
 */
    y = y-.1;
    text_align.hor = GHOR_LEFT;
    text_align.vert = GVERT_HALF;
    gset_text_align(&text_align);
    pos.x = x;
    pos.y = y;
    gtext(&pos,"Alignment = (1,3)");
    cross(x,y);
/*
 *  Alignment = (2,3) [center, center]
 */
    y = y-.1;
    text_align.hor = GHOR_CTR;
    text_align.vert = GVERT_HALF;
    gset_text_align(&text_align);
    pos.x = x;
    pos.y = y;
    gtext(&pos,"Alignment = (2,3)");
    cross(x,y);
/*
 *  Alignment = (3,3) [ right, center]
 */
    y = y-.1;
    text_align.hor = GHOR_RIGHT;
    text_align.vert = GVERT_HALF;
    gset_text_align(&text_align);
    pos.x = x;
    pos.y = y;
    gtext(&pos,"Alignment = (3,3)");
    cross(x,y);
/*
 *  Alignment = (1,1) [  left,    top]
 */
    x = .25;
    y = y-.1;
    text_align.hor = GHOR_LEFT;
    text_align.vert = GVERT_TOP;
    gset_text_align(&text_align);
    pos.x = x;
    pos.y = y;
    gtext(&pos,"Alignment = (1,1)");
    cross(x,y);
/*
 *  Alignment = (1,2) [  left,    cap]
 */
    y = y-.1;
    text_align.hor = GHOR_LEFT;
    text_align.vert = GVERT_CAP;
    gset_text_align(&text_align);
    pos.x = x;
    pos.y = y;
    gtext(&pos,"Alignment = (1,2)");
    cross(x,y);
/*
 *  Alignment = (1,3) [  left, center]
 */
    y = y-.1;
    text_align.hor = GHOR_LEFT;
    text_align.vert = GVERT_HALF;
    gset_text_align(&text_align);
    pos.x = x;
    pos.y = y;
    gtext(&pos,"Alignment = (1,3)");
    cross(x,y);
/*
 *  Alignment = (1,4) [  left,   base]
 */
    y = y-.1;
    text_align.hor = GHOR_LEFT;
    text_align.vert = GVERT_BASE;
    gset_text_align(&text_align);
    pos.x = x;
    pos.y = y;
    gtext(&pos,"Alignment = (1,4)");
    cross(x,y);
/*
 *  Alignment = (1,5) [  left, bottom]
 */
    y = y-.1;
    text_align.hor = GHOR_LEFT;
    text_align.vert = GVERT_BOTTOM;
    gset_text_align(&text_align);
    pos.x = x;
    pos.y = y;
    gtext(&pos,"Alignment = (1,5)");
    cross(x,y);
/*
 *  Label the plot (PLOTCHAR strokes its characters with lines so the
 *  character color is controlled by the line color).
 */
    gset_line_colr_ind(2);
    c_pcseti("CD",1);
    gset_linewidth(2.);
    c_plchhq(.5,.93,"Text Alignment Attributes",.019,0.,0.);
    x = .20; y = .88;
    cross(x,y);
    c_plchhq(.5,.88,"- marks the GTX coordinate",.019,0.,0.);
    gclear_ws(1,1);
/*
 *  Deactivate and close the workstation, close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

cross(x,y)
float x, y;
{
/*
 *  Draw a filled cross at coordinate (X,Y) using color index 3.
 */
    int i, icx[ID],icy[ID];
    Gpoint_list     area;

    icx[ 0] =     0;
    icx[ 1] =     IX;
    icx[ 2] =   IMXH;
    icx[ 3] =  IMXM;
    icy[ 0] =     0;
    icy[ 1] =      0;
    icy[ 2] =  IMXHM;
    icy[ 3] =     0;
    icx[ 4] =   IMX;
    icx[ 5] =    IMX;
    icx[ 6] =  IMXHP;
    icx[ 7] =   IMX;
    icy[ 4] =     0;
    icy[ 5] =     IX;
    icy[ 6] =   IMXH;
    icy[ 7] =  IMXM;
    icx[ 8] =   IMX;
    icx[ 9] =   IMXM;
    icx[10] =   IMXH;
    icx[11] =    IX;
    icy[ 8] =   IMX;
    icy[ 9] =    IMX;
    icy[10] =  IMXHP;
    icy[11] =   IMX;
    icx[12] =     0;
    icx[13] =      0;
    icx[14] =  IMXHM;
    icx[15] =     0;
    icy[12] =   IMX;
    icy[13] =   IMXM;
    icy[14] =   IMXH;
    icy[15] =    IX;
    gset_fill_int_style (GSTYLE_SOLID);
    gset_fill_colr_ind(3);
    area.num_points = ID;
    area.points = (Gpoint *)calloc(ID,sizeof(Gpoint));
    for( i = 0; i < ID; i++ ) {
        area.points[i].x = x-0.00025*((float)(IMXH)-(float)(icx[i]));
        area.points[i].y = y-0.00025*((float)(IMXH)-(float)(icy[i]));
    }
    gfill_area(&area);
    return(1);
}
