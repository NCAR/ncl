/*
 *  $Id: c_coex02.c,v 1.8 1999-07-27 20:15:02 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

/*
 *  Number of squares in the X and Y directions.
 */
int NX= 4;
int NY=4;
/*
 *  Size of each square; spacing between squares.
 */
float SZX= .235,SZY=.135,Y0=.10,Y1=.88;

#define IWTYPE 1
#define WKID   1

main()
{
/*
 *  Arrays to store labels.
 */
      char tlab[4][4][26],blab[4][4][21];
      float SPX, SPY;
/*
 *  Color value array.
 */
      Gcolr_rep rgb[4][4], rgb2;
      int i, j;
      float x, y;

      SPX=(1.-NX*SZX)/(NX+1);
      SPY=(Y1-Y0-NY*SZY)/(NY-1);

      rgb[0][0].rgb.red = 0.86; rgb[0][0].rgb.green = 0.58; rgb[0][0].rgb.blue = 0.44;
      rgb[1][0].rgb.red = 0.65; rgb[1][0].rgb.green = 0.16; rgb[1][0].rgb.blue = 0.16;
      rgb[2][0].rgb.red = 1.00; rgb[2][0].rgb.green = 0.50; rgb[2][0].rgb.blue = 0.00;
      rgb[3][0].rgb.red = 1.00; rgb[3][0].rgb.green = 0.00; rgb[3][0].rgb.blue = 0.00;
      rgb[0][1].rgb.red = 1.00; rgb[0][1].rgb.green = 1.00; rgb[0][1].rgb.blue = 0.00;
      rgb[1][1].rgb.red = 0.00; rgb[1][1].rgb.green = 1.00; rgb[1][1].rgb.blue = 0.00;
      rgb[2][1].rgb.red = 0.14; rgb[2][1].rgb.green = 0.56; rgb[2][1].rgb.blue = 0.14;
      rgb[3][1].rgb.red = 0.00; rgb[3][1].rgb.green = 1.00; rgb[3][1].rgb.blue = 1.00;
      rgb[0][2].rgb.red = 0.20; rgb[0][2].rgb.green = 0.56; rgb[0][2].rgb.blue = 0.80;
      rgb[1][2].rgb.red = 0.00; rgb[1][2].rgb.green = 0.00; rgb[1][2].rgb.blue = 1.00;
      rgb[2][2].rgb.red = 0.50; rgb[2][2].rgb.green = 0.00; rgb[2][2].rgb.blue = 1.00;
      rgb[3][2].rgb.red = 1.00; rgb[3][2].rgb.green = 0.00; rgb[3][2].rgb.blue = 1.00;
      rgb[0][3].rgb.red = 1.00; rgb[0][3].rgb.green = 1.00; rgb[0][3].rgb.blue = 1.00;
      rgb[1][3].rgb.red = 0.66; rgb[1][3].rgb.green = 0.66; rgb[1][3].rgb.blue = 0.66;
      rgb[2][3].rgb.red = 0.40; rgb[2][3].rgb.green = 0.40; rgb[2][3].rgb.blue = 0.40;
      rgb[3][3].rgb.red = 0.00; rgb[3][3].rgb.green = 0.00; rgb[3][3].rgb.blue = 0.00;

      strcpy(tlab[0][0], "Tan                      " );
      strcpy(tlab[1][0], "Brown                    " );
      strcpy(tlab[2][0], "Orange                   " );
      strcpy(tlab[3][0], "Red                      " );
      strcpy(tlab[0][1], "Yellow                   " );
      strcpy(tlab[1][1], "Green                    " );
      strcpy(tlab[2][1], "Forest Green             " );
      strcpy(tlab[3][1], "Cyan                     " );
      strcpy(tlab[0][2], "Sky Blue                 " );
      strcpy(tlab[1][2], "Blue                     " );
      strcpy(tlab[2][2], "Blue Magenta             " );
      strcpy(tlab[3][2], "Magenta                  " );
      strcpy(tlab[0][3], "White                    " );
      strcpy(tlab[1][3], "Light Gray               " );
      strcpy(tlab[2][3], "Dark Gray                " );
      strcpy(tlab[3][3], "Black                    " );

      gopen_gks ("stdout",0);
      gopen_ws (WKID, NULL, IWTYPE);
      gactivate_ws(WKID);
/*
 *  Use the Duplex character set of PLOTCHAR.
 */
      c_pcseti("CD",1);
/*
 *  Define color indices and RGB labels..
 */
      rgb2.rgb.red = rgb2.rgb.green = rgb2.rgb.blue = 0.;
      gset_colr_rep(WKID,0,&rgb2);
      rgb2.rgb.red = rgb2.rgb.green = rgb2.rgb.blue = 1.;
      gset_colr_rep(WKID,1,&rgb2);
      for( j = 0; j < NY; j++ ) {
          for( i = 0; i < NX; i++ ) {
              gset_colr_rep(WKID,NX*(j)+i+2,&rgb[i][j]);
              sprintf(blab[i][j], "R=%4.2f G=%4.2f B=%4.2f", rgb[i][j].rgb.red,rgb[i][j].rgb.green,rgb[i][j].rgb.blue);
          }
      }
/*
 *  Draw the color squares and titles.
 */
      for( j = 0; j < NY; j++ ) {
          y = Y0+(j)*(SPY+SZY);
          for( i = 0; i < NX; i++ ) {
              x = SPX+(i)*(SPX+SZX);
              drbox(x,y,SZX,Y0,SZY,tlab[i][j],blab[i][j],NX*(j)+i+2);
          }
      }
/*
 *  Plot labels.
 */
      c_plchhq(.5,.04,"The titles below each box indicate Red, Green and Blue intensity values.",.012,0.,0.);
      c_pcseti("CD",1);
      c_plchhq(.5,.96,"Sixteen Sample Colors",.02,0.,0.);
      c_frame();
      gdeactivate_ws(WKID);
      gclose_ws(WKID);
      gclose_gks();
}

drbox (x,y,szx,y0,szy,tlab,blab,indx)
char *tlab, *blab;
float x, y, szx, y0, szy;
int indx;
{
/*
 *  draw a color square with lower left corner (x,y)
 */
    float cr, cg, cb;
    int ier, ilen, k, itlen, iblen;
    Gpoint_list fill_area;
    Gcolr_rep rgb;

    rgb.rgb.red = rgb.rgb.green = rgb.rgb.blue = 1.;
    gset_colr_rep(WKID,1,&rgb);
    gset_fill_colr_ind(indx);
    gset_fill_int_style (GSTYLE_SOLID);
/*
 * Create structure to pass to gfill_area
 */
    fill_area.num_points = 5;
    fill_area.points = (Gpoint *) malloc(5*sizeof(Gpoint));
    if( !fill_area.points ) {
        fprintf( stderr, "drbox: Not enough memory to create fill area structure\n" );
        gemergency_close_gks();
        exit(1);
    }
    fill_area.points[0].x = x;
    fill_area.points[0].y = y;
    fill_area.points[1].x = x+szx;
    fill_area.points[1].y = y;
    fill_area.points[2].x = fill_area.points[1].x;
    fill_area.points[2].y = y+szy;
    fill_area.points[3].x = x;
    fill_area.points[3].y = fill_area.points[2].y;
    fill_area.points[4].x = fill_area.points[0].x;
    fill_area.points[4].y = fill_area.points[0].y;
    gfill_area(&fill_area);
/*
 *  if the color is black, draw a boundary.
 */
    ginq_colr_rep(WKID,indx,GINQ_SET,&ier,&rgb);
    fill_area.num_points = 5;
    if (rgb.rgb.red == 0. && rgb.rgb.green == 0. && rgb.rgb.blue == 0.) {
        gset_line_colr_ind(1);
        gpolyline(&fill_area);
    }
    free(fill_area.points);
    ilen = strlen(tlab);
    for( k=ilen; k >= 1; k-- ) {
        if (tlab[k-1] != ' ') {
            itlen = k;
            break;
        }
    }
    ilen = strlen(blab);
    for( k=ilen; k >= 1; k-- ) {
        if (blab[k-1] != ' ') {
            iblen = k;
            break;
        }
    }
    gset_line_colr_ind(1);
    blab[iblen] = '\0';
    tlab[itlen] = '\0';
    c_plchhq(x+.5*szx,y-.015,blab,.0098,0.,0.);
    c_plchhq(x+.5*szx,y+szy+.017,tlab,.012,0.,0.);
    return(1);
}
