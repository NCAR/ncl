/*
 *  Illustrate GKS text path and color.
 */

#include <stdio.h>
#include <stdlib.h>

/*
 * Include function prototypes here
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

/*
 * Set up the DEFINES
 */
#define ID    16
#define IX    15
#define IMX   100
#define IMXH  IMX/2
#define IMXM  IMX-IX
#define IMXP  IMX+IX
#define IMXHM IMXH-IX
#define IMXHP IMXH+IX

#define IWTYPE 1
#define WKID   1

main()
{
    Gcolr_rep       rgbs[7];
    Gtext_font_prec tfp;
    Gtext_align     text_align;
    Gpoint          pos;
    float x,y;
    int i;
#ifdef NeedFuncProto
    extern void cross(float,float);
#else
    extern void cross();
#endif
/*
 *  Open GKS, open and activate the metafile workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws (WKID, NULL, IWTYPE);
    gactivate_ws(WKID);
/*
 *  Define the necessary color indices.
 */
    rgbs[0].rgb.red = 0.;    rgbs[0].rgb.green = 0.;    rgbs[0].rgb.blue = .6;
    rgbs[1].rgb.red = 1.;    rgbs[1].rgb.green = 1.;    rgbs[1].rgb.blue = 1.;
    rgbs[2].rgb.red = 1.;    rgbs[2].rgb.green = 0.;    rgbs[2].rgb.blue = 0.;
    rgbs[3].rgb.red = 0.;    rgbs[3].rgb.green = 1.;    rgbs[3].rgb.blue = 0.;
    rgbs[4].rgb.red = 1.;    rgbs[4].rgb.green = 1.;    rgbs[4].rgb.blue = 0.;
    rgbs[5].rgb.red = 0.;    rgbs[5].rgb.green = 1.;    rgbs[5].rgb.blue = 1.;
    rgbs[6].rgb.red = 1.;    rgbs[6].rgb.green = 0.;    rgbs[6].rgb.blue = 1.;
    for( i = 0; i <= 6; i++ ) {
        gset_colr_rep(WKID,i,&rgbs[i]);
    }
/*
 *  Select Triplex Roman font.
 */
    tfp.font = -13;
    tfp.prec = GPREC_STROKE;
    gset_text_font_prec(&tfp);
/*
 *  Text path = right, color = yellow
 */
    x = .2;
    y = .7;
    gset_char_ht(.04);
    gset_text_path(GPATH_RIGHT);
    gset_text_colr_ind(4);
    text_align.hor = GHOR_LEFT;
    text_align.vert = GVERT_HALF;
    gset_text_align(&text_align);
    pos.x = x;
    pos.y = y;
    gtext(&pos,"Text path=right");
    cross(x,y);
/*
 *  Text path = left, color = red
 */
    x = .80;
    y = .115;
    gset_char_ht(.04);
    gset_text_path(GPATH_LEFT);
    gset_text_colr_ind(2);
    text_align.hor = GHOR_RIGHT;
    text_align.vert = GVERT_HALF;
    gset_text_align(&text_align);
    pos.x = x;
    pos.y = y;
    gtext(&pos,"Text path=left");
    cross(x,y);
/*
 *  Text path = down, color = cyan
 */
    x = .22;
    y = .62;
    gset_char_ht(.025);
    gset_text_path(GPATH_DOWN);
    gset_text_colr_ind(5);
    text_align.hor = GHOR_CTR;
    text_align.vert = GVERT_TOP;
    gset_text_align(&text_align);
    pos.x = x;
    pos.y = y;
    gtext(&pos,"Text path=down");
    cross(x,y);
/*
 *  Text path = up, color = magenta
 */
    x = .79;
    y = .18;
    gset_char_ht(.03);
    gset_text_path(GPATH_UP);
    gset_text_colr_ind(6);
    text_align.hor = GHOR_CTR;
    text_align.vert = GVERT_BOTTOM;
    gset_text_align(&text_align);
    pos.x = x;
    pos.y = y;
    gtext(&pos,"Text path=up");
    cross(x,y);
/*
 *  Label the plot (PLOTCHAR strokes its characters with lines, so the
 *  PLOTCHAR character attributes are controlled by the GKS polyline 
 *  attributes.
 */
    gset_line_colr_ind(4);
    gset_linewidth(2.);
    c_pcseti("CD",1);
    c_plchhq(.5,.93,"Text Colors and Paths",.020,0.,0.);
    c_plchhq(.5,.88,"Font = triplex Roman",.020,0.,0.);
    x = .2; y = .83;
    cross(x,y);
    c_plchhq(.5,.83,"- marks the GTX coordinate",.020,0.,0.);
/*
 * Call frame.
 */
    c_frame();
/*
 *  Deactivate and close the workstation, close GKS.
 */
    gdeactivate_ws (WKID);
    gclose_ws (WKID);
    gclose_gks();
}


/*
 *  Draw a green filled cross at coordinate (X,Y) using color index 3.
 */
void cross
#ifdef NeedFuncProto
(float x,float y)
#else
(x,y)
float x, y;
#endif
{
    int i, icx[ID],icy[ID];
    Gpoint_list     area;

    icx[ 0] =     0;
    icx[ 1] =    IX;
    icx[ 2] =  IMXH;
    icx[ 3] =  IMXM;
    icy[ 0] =     0;
    icy[ 1] =     0;
    icy[ 2] = IMXHM;
    icy[ 3] =     0;
    icx[ 4] =   IMX;
    icx[ 5] =   IMX;
    icx[ 6] = IMXHP;
    icx[ 7] =   IMX;
    icy[ 4] =     0;
    icy[ 5] =    IX;
    icy[ 6] =  IMXH;
    icy[ 7] =  IMXM;
    icx[ 8] =   IMX;
    icx[ 9] =  IMXM;
    icx[10] =  IMXH;
    icx[11] =    IX;
    icy[ 8] =   IMX;
    icy[ 9] =   IMX;
    icy[10] = IMXHP;
    icy[11] =   IMX;
    icx[12] =     0;
    icx[13] =     0;
    icx[14] = IMXHM;
    icx[15] =     0;
    icy[12] =   IMX;
    icy[13] =  IMXM;
    icy[14] =  IMXH;
    icy[15] =    IX;
/*
 * Set interior fill style and color
 */
    gset_fill_int_style(GSTYLE_SOLID);
    gset_fill_colr_ind(3);
/*
 *  Malloc space for cross
 */
    area.num_points = ID;
    area.points = (Gpoint *)malloc(area.num_points*sizeof(Gpoint));
    if( !area.points ) {
        fprintf( stderr, "cross:  Not enough memory to create fill area arrays\n" );
        return;
    }
    for( i = 0; i < ID; i++ ) {
        area.points[i].x = x-0.00025*((float)(IMXH)-(float)(icx[i]));
        area.points[i].y = y-0.00025*((float)(IMXH)-(float)(icy[i]));
    }
/*
 * Draw the filled area
 */
    gfill_area(&area);
/*
 * Free up memory
 */
    free(area.points);
    return;
}
