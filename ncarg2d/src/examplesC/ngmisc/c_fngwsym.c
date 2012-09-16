/*
 *  $Id: c_fngwsym.c,v 1.5 1999-07-27 20:15:05 haley Exp $
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

char *cfnt[7] = {"a ","N ","W ","CH","CM","CL","C" };

char *ylab[2][7] = {"a","N","W","CH","CM","CL","C","Pressure Tendency ",
                    "Sky Cover         ","Past Weather      ",
                    "High Clouds      ","Medium Clouds    ",
                    "Low Clouds       ","Cloud Types       "};

int ylen[2][7] = {1,1,1,2,2,2,1,17,9,12,11,13,10,11};

#define IWTYPE 1
#define WKID   1

main()
{
/*
 *  Plots tables of all the weather symbols.
 *
 *  Open GKS, set color table and line width.
 */
    Gcolr_rep rgb;
    float x1, x2, y1, y2, y3;
    float size, yy, r, dy, spc, yb, yinc;
    int i, j, nc, nr, nf;
    char stmp1[20], stmp2[20];
#ifdef NeedFuncProto
	extern void dtable(char *,int,int,float,float,float,float,float,int,int,int);
	extern void nputs(float,char *,char *,float);
#else
	extern void dtable();
    extern void nputs();
#endif

	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, IWTYPE);
	gactivate_ws(WKID);

    rgb.rgb.red = 1.;    rgb.rgb.green = 1.;    rgb.rgb.blue = 1.;
    gset_colr_rep(WKID,0,&rgb);
    rgb.rgb.red = 0.;    rgb.rgb.green = 0.;    rgb.rgb.blue = 0.;
    gset_colr_rep(WKID,1,&rgb);
    rgb.rgb.red = 1.;    rgb.rgb.green = 1.;    rgb.rgb.blue = 0.;
    gset_colr_rep(WKID,2,&rgb);
    gset_linewidth(1.);
/*
 *  Font WW -- Present Weather table.
 *
 *  Table width.
 */;
    x1 = .025;
    x2 = .975;
/*
 *  Table heights.
 */  
    y1 = 0.;
    y2 = .95;
    y3 = 1.;
/*
 *  Number of columns and rows.
 */
    nc = 10;
    nr = 10;

    gset_line_colr_ind(1);
    gset_text_colr_ind(1);
    dtable("WW",nr,nc,x1,y1,x2,y2,y3,1,1,1);
    size = .4*(y3-y2);
    yy = .5*(y2+y3);
    nputs(yy,"WW","Present Weather",size);
    c_frame();

    nr = 1;
    nc = 10;
    r = 4.5/12.;
    dy = .095;
    spc = 1./7.-(1.+r)*dy;
    for( nf = 1; nf <= 7; nf++ ) {
        yb = (nf-1)*((1.+r)*dy+spc);
        y1 = yb;
        y2 = yb+dy;
        y3 = yb+(1+r)*dy;
        dtable(cfnt[nf-1],nr,nc,x1,y1,x2,y2,y3,1,1,1);
        yy = .5*(y2+y3);
        strncpy( stmp1, ylab[0][nf-1], ylen[0][nf-1] );
        strncpy( stmp2, ylab[1][nf-1], ylen[1][nf-1] );
        stmp1[ylen[0][nf-1]] = '\0';
        stmp2[ylen[1][nf-1]] = '\0';
        nputs(yy,stmp1,stmp2,.5*(y3-y2));
    }
    c_frame();

	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void dtable
#ifdef NeedFuncProto
(char *font,int nr,int nc,float x1,float y1,float x2,float y2,float y3,int itx1,int itx2,int ilnc)
#else
(font,nr,nc,x1,y1,x2,y2,y3,itx1,itx2,ilnc)
char *font;
int nr, nc;
float x1, y1, x2, y2, y3;
int itx1, itx2, ilnc;
#endif
{
/*
 *  Draw table of characters from font FONT with NR rows and NC columns.
 *  The boundary of the table in world coordinates is (X1,Y1) to (X2,Y2).
 *  The line color index is ILNC and the text color index for the label
 *  characters is ITX1 and for the displayed characters is ITX2.
 */
    float xc[10],yc[10],xlc[10],ylc[10];
    float xinc, yinc, chgt;
    int i, j, num, ier;
    char nlab[5];
    Gtext_align text_align;
    Gtext_font_prec text_font_prec;
    Gpoint_list polyline;
/*
 * Initialize polyline
 */
    polyline.num_points = 2;
    polyline.points = (Gpoint *)malloc(2*sizeof(Gpoint));
/*
 *  Draw table grid and compute the centers of the grid boxes.
 */
    gset_linewidth(2.);
    gset_line_colr_ind(ilnc);
    yinc = (y2-y1)/(float)nr;
    for( j = 1; j <= nr+1; j++ ) {
        polyline.points[0].y = y1+(j-1)*yinc;
        polyline.points[1].y = polyline.points[0].y;
        polyline.points[0].x = x1;
        polyline.points[1].x = x2;
        gpolyline(&polyline);
        if (j <= nr) {
            yc[j-1] = polyline.points[0].y+.4*yinc;
            ylc[j-1] = polyline.points[0].y+.8*yinc;
        }
    }

    xinc = (x2-x1)/(float)nc;
    for( i = 1; i <= nc+1; i++ ) {
        polyline.points[0].y = y1;
        polyline.points[1].y = y2;
        polyline.points[0].x = x1+(i-1)*xinc;
        polyline.points[1].x = polyline.points[0].x;
        gpolyline(&polyline);
        if (i <= nc) {
            xc[i-1] = polyline.points[0].x+.6*xinc;
            xlc[i-1] = polyline.points[0].x+.2*xinc;
        }
    }
    gset_linewidth(1.);
/*
 *  draw the characters in the boxes.
 */
    text_align.hor = GHOR_CTR;
    text_align.vert = GVERT_HALF;
    gset_text_align(&text_align);
    text_font_prec.font = -36;
    text_font_prec.prec = GPREC_STROKE;
    gset_text_font_prec(&text_font_prec);
    gset_text_colr_ind(itx2);
    for( j = 1; j <= nr; j++ ) {
        for( i = 1; i <= nc; i++ ) {
            num = (nr-j)*nc+i-1;
            c_ngwsym(font,num,xc[i-1],yc[j-1],.43*yinc,1,0);
        }
    }
/*    
 *  draw the character number in the upper left of the box.
 */
    chgt = .14*yinc;
    for( j = 1; j <= nr; j++ ) {
        for( i = 1; i <= nc; i++ ) {
            num = (nr-j)*nc+i-1;
            sprintf( nlab, "%2d", num );
            c_pcseti("fn",26);
            c_pcseti("cc",itx1);
            c_plchhq(xlc[i-1],ylc[j-1],nlab,6.*chgt/7.,0.,0.);
        }
    }
}

void nputs
#ifdef NeedFuncProto
(float yy,char *str1,char *str2,float size)
#else
(yy,str1,str2,size)
float yy;
char *str1, *str2;
float size;
#endif
{
/*
 *  put out str1, then a dash, then str2 at size size.
 */
    float strtx;
    float dx1, dx2, dx3, dx;
    Gint ier;
    Gtext_font_prec text_font_prec;
    Gtext_align text_align;
    Gpoint pos;
    Gtext_extent text_extent;

    text_font_prec.font = -22;
    text_font_prec.prec = GPREC_STROKE;
    gset_text_font_prec(&text_font_prec);
    text_align.hor = GHOR_LEFT;
    text_align.vert = GVERT_CAP;
    gset_text_align(&text_align);
    gset_char_ht(size);
    pos.x = 0.;
	pos.y = yy;
    ginq_text_extent(1,&pos,str1,&ier,&text_extent);
    dx1 = text_extent.concat_point.x;
    ginq_text_extent(1,&pos,str2,&ier,&text_extent);
    dx3 = text_extent.concat_point.x;
    text_font_prec.font = -34;
    text_align.vert = GVERT_CAP;
    gset_text_font_prec(&text_font_prec);
    ginq_text_extent(1,&pos," > ",&ier,&text_extent);
    dx2 = text_extent.concat_point.x;
    dx = dx1+dx2+dx3;
    strtx = .5-.5*dx;
    c_pcseti("fn",22);
    c_plchhq(strtx,yy,str1,6.*size/7.,0.,-1.);
    c_plchhq(strtx+dx1+dx2,yy,str2,6.*size/7.,0.,-1.);
    text_font_prec.font = -34;
    gset_text_font_prec(&text_font_prec);
    c_pcseti("fn",34);
    c_plchhq(strtx+dx1,yy," > ",6.*size/7.,0.,-1.);
}
