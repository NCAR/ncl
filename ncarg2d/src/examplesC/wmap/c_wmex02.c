/*
** examples of spline fits for weather fronts and regions.
**
** define error file, fortran unit number, and workstation type,
** and workstation id.
*/

#include <stdio.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>

#define LUNIT "gmeta"
#define IWTYPE SED_WSTYPE
#define WKID 1

#define NS 3 
#define NT 5 
#define NU 8 
#define NV 5 

main()
{
      int i;
      float xb;
      Gcolr_rep rgb;
      void cross();
/*
** color index for cross markers.
*/
      int icclr=3;
/*
** data for two lines and a region illustrating spline fits.
*/
      float xs[NS],ys[NS];
      float xt[NT],yt[NT];
      float xu[NU],yu[NU];
/*
** data for picture two illustrating slope control at end points.
*/
      float xv[NV],yv[NV];
      
      xs[0] = 0.1; 
      xs[1] = 0.3; 
      xs[2] = 0.5;

      ys[0] = 0.45; 
      ys[1] = 0.7; 
      ys[2] = 0.75;

      xt[0] = 0.15; 
      xt[1] = 0.2; 
      xt[2] = 0.5; 
      xt[3] = 0.7; 
      xt[4] = 0.85;

      yt[0] = 0.05; 
      yt[1] = 0.28; 
      yt[2] = 0.53; 
      yt[3] = 0.58; 
      yt[4] = 0.75;

      xu[0] = 0.35; 
      xu[1] = 0.4; 
      xu[2] = 0.6; 
      xu[3] = 0.8; 
      xu[4] = 0.85;
      xu[5] = 0.7; 
      xu[6] = 0.5; 
      xu[7] = 0.35;

      yu[0] = 0.1;
      yu[1] = 0.3;
      yu[2] = 0.31;
      yu[3] = 0.43;
      yu[4] = 0.15;
      yu[5] = 0.1;
      yu[6] = 0.05;
      yu[7] = 0.1;

      xv[0] = 0.1;
      xv[1] = 0.3;
      xv[2] = 0.5;
      xv[3] = 0.7;
      xv[4] = 0.9;

      yv[0] = 1.0;
      yv[1] = 1.08;
      yv[2] = 1.0;
      yv[3] = 0.95;
      yv[4] = 0.94;
/*
** open gks, open and activate a workstation.
*/
      gopen_gks ("stdout", 0);
      gopen_ws (WKID, LUNIT, IWTYPE);
      gactivate_ws (WKID);
/*
** define a color table.
*/
      rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
      gset_colr_rep(WKID,0,&rgb);
      rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
      gset_colr_rep(WKID,1,&rgb);
      rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
      gset_colr_rep(WKID,2,&rgb);
      rgb.rgb.red = 0.0; rgb.rgb.green = 0.0; rgb.rgb.blue = 1.;
      gset_colr_rep(WKID,3,&rgb);
/*
** plot title.
*/
      c_plchhq(0.50,0.94,":F26:Spline fits for fronts and regions",0.03,0.,0.);
      c_plchhq(0.50,0.88,":F22: - marks the input coordinates",.025,0.,-.06);
      c_pcgetr("xb",&xb);
      cross(xb-.015,0.875,icclr);
/*
** define some parameters.
*/
      c_wmsetr("LIN - line widths of front lines",3.);
      c_wmseti("NMS - number of symbols on front line",0);
      c_wmseti("WFC - color for warm fronts",2);
 
      c_wmdrft(NS,xs,ys);
      for(i=0; i<NS; ++i)
      {
        cross(xs[i],ys[i],icclr);
      }
      c_wmdrft(NT,xt,yt);
      for(i=0; i<NT; ++i)
      {
        cross(xt[i],yt[i],icclr);
      }
      c_wmdrrg(NU,xu,yu,"index2",1,xu,yu);
      for(i=0; i<NU; ++i)
      {
        cross(xu[i],yu[i],icclr);
      }
 
      c_frame();

      gdeactivate_ws (WKID);
      gclose_ws (WKID);
      gclose_gks();
}


#define ID 16 
#define IX 15
#define IMX 100
#define IMXH IMX/2 
#define IMXM IMX-IX  
#define IMXHM IMXH-IX
#define IMXHP IMXH+IX 


void cross(x,y,iclr)
float x;
float y;
int iclr;
{
/*
** draw a green filled cross at (x,y).
*/
      int i;
      int err_ind;
      int ioc;
      int icx[ID],icy[ID];
      Gpoint_list point_list;
 
      icx[0] = 0;
      icx[1] = IX;
      icx[2] = IMXH;
      icx[3] = IMXM;
      icx[4] = IMX;
      icx[5] = IMX; 
      icx[6] = IMXHP;
      icx[7] = IMX;
      icx[8] = IMX;
      icx[9] = IMXM;
      icx[10] = IMXH;
      icx[11] = IX; 
      icx[12] = 0;
      icx[13] = 0;
      icx[14] = IMXHM;
      icx[15] = 0;

      icy[0] = 0;
      icy[1] = 0;
      icy[2] = IMXHM;
      icy[3] = 0;
      icy[4] = 0;
      icy[5] = IX;
      icy[6] = IMXH;
      icy[7] = IMXM;
      icy[8] = IMX;
      icy[9] = IMX;
      icy[10] = IMXHP;
      icy[11] = IMX;
      icy[12] = IMX;
      icy[13] = IMXM;
      icy[14] = IMXH;
      icy[15] = IX;

      point_list.num_points = ID;
      point_list.points = (Gpoint*)malloc(point_list.num_points*sizeof(Gpoint));
      for(i=0; i<ID; ++i)
      {
        point_list.points[i].x = x-0.00027*((float)(IMXH)-(float)(icx[i]));
        point_list.points[i].y = y-0.00027*((float)(IMXH)-(float)(icy[i]));
      }

      gset_fill_int_style(1);
      ginq_fill_colr_ind(&err_ind, &ioc);
      gset_fill_colr_ind(iclr);
      gfill_area(&point_list);
      gset_fill_colr_ind(ioc);
      free (point_list.points);
} 
