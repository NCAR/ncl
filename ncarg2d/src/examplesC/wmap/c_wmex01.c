/*
**  example of weather fronts.
**
**  define error file, fortran unit number, and workstation type,
**  and workstation id.
*/

#include <stdio.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>

#define LUNIT "gmeta"
#define IWTYPE SED_WSTYPE
#define WKID 1
 

main()
{
      float x[2],y[2];

      float dely = 0.09;
      float xp = 0.375; 
      float alsiz = 0.024;

      Gcolr_rep rgb;

      x[0] = 0.40; 
      x[1] = 0.90;

      y[0] = 0.85; 
      y[1] = 0.85;
/*
**  open gks, open and activate a workstation.
*/
      gopen_gks ("stdout", 0);
      gopen_ws (WKID, LUNIT, IWTYPE);
      gactivate_ws (WKID);
/*
**  define a color table.
*/
      rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
      gset_colr_rep(WKID,0,&rgb);
      rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
      gset_colr_rep(WKID,1,&rgb);
      rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
      gset_colr_rep(WKID,2,&rgb);
      rgb.rgb.red = 0.5; rgb.rgb.green = 0.0; rgb.rgb.blue = 0.7;
      gset_colr_rep(WKID,3,&rgb);
      rgb.rgb.red = 0.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
      gset_colr_rep(WKID,4,&rgb);
      rgb.rgb.red = 1.; rgb.rgb.green = 0.5; rgb.rgb.blue = 0.;
      gset_colr_rep(WKID,5,&rgb);
      rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
      gset_colr_rep(WKID,6,&rgb);
/*
**  plot title.
*/
      c_plchhq(0.5,0.95,":F26:Front Types",.035,0.,0.);
/*
**  define some parameters.
*/
      c_wmsetr("LIN - line widths of warm/cold front lines",3.);
      c_wmsetr("BEG - space before first front symbol",0.02);
      c_wmsetr("END - space after last front symbol",0.02);
      c_wmsetr("BET - space between front symbols",.03);
      c_wmsetr("SWI - size of symbols on front line",.04);
      c_wmseti("WFC - color for warm front symbols",2);
      c_wmseti("CFC - color for cold front symbols",6);
/*
**  warm front.
*/
      c_wmsetc("FRO - front type","warm");
      c_wmseti("REV - reverse the direction of the symbols",1);
      c_wmdrft(2,x,y);      
      c_plchhq(xp,y[0],":F22:Warm",alsiz,0.,1.);
      
/*
**  warm front, aloft.
*/
      y[0] = y[0]-dely;
      y[1] = y[0];
      c_wmsetc("FRO - fornt type","warm");
      c_wmseti("ALO - specify aloft",1);
      c_wmseti("REV - reverse the direction of the symbols",1);
      c_wmdrft(2,x,y);      
      c_plchhq(xp,y[0],":F22:Warm, aloft",alsiz,0.,1.);
      c_wmseti("ALO - deactivate aloft flag",0);
/*
**  cold front.
*/
      y[0] = y[0]-dely;
      y[1] = y[0];
      c_wmsetc("FRO - front type","cold");
      c_wmseti("REV - reverse the direction of the symbols",1);
      c_wmdrft(2,x,y);      
      c_plchhq(xp,y[0],":F22:Cold",alsiz,0.,1.);
/*
**  cold front, aloft.
*/
      y[0] = y[0]-dely;
      y[1] = y[0];
      c_wmsetc("FRO - front type","cold");
      c_wmseti("ALO - specify aloft",1);
      c_wmseti("REV - reverse the direction of the symbols",1);
      c_wmdrft(2,x,y);      
      c_plchhq(xp,y[0],":F22:Cold, aloft",alsiz,0.,1.);
      c_wmseti("ALO - deactivate aloft flag",0);
/*
**  stationary front.
*/
      y[0] = y[0]-dely;
      y[1] = y[0];
      c_wmsetc("FRO - front type","stationary");
      c_wmseti("REV - reverse the direction of the symbols",1);
      c_wmdrft(2,x,y);      
      c_plchhq(xp,y[0],":F22:Stationary",alsiz,0.,1.);
/*
**  stationary front, aloft.
*/
      y[0] = y[0]-dely;
      y[1] = y[0];
      c_wmsetc("FRO - front type","stationary");
      c_wmseti("REV - reverse the direction of the symbols",1);
      c_wmseti("ALO - specify aloft",1);
      c_wmdrft(2,x,y);      
      c_plchhq(xp,y[0],":F22:Stationary, aloft",alsiz,0.,1.);
      c_wmseti("ALO - deactivate aloft flag",0);
/*
**  occluded front.
*/
      y[0] = y[0]-dely;
      y[1] = y[0];
      c_wmseti("WFC - color for warm front symbols",3);
      c_wmseti("CFC - color for cold front symbols",3);
      c_wmsetc("FRO - front type","occluded");
      c_wmseti("REV - reverse the direction of the symbols",1);
      c_wmdrft(2,x,y);      
      c_plchhq(xp,y[0],":F22:Occluded",alsiz,0.,1.);
 
      c_wmsetr("dwd - line widths for fronts with no symbols",3.);
/*
**  convergence line
*/
      y[0] = y[0]-dely;
      y[1] = y[0];
      c_wmsetc("FRO - front type","convergence");
      c_wmseti("COL - convergence lines are orange",5);
      c_wmdrft(2,x,y);      
      c_plchhq(xp,y[0],":F22:Convergence line",alsiz,0.,1.);
/*
**  instability line.
*/
      y[0] = y[0]-dely;
      y[1] = y[0];
      c_wmsetc("FRO - front type","squall");
      c_wmseti("COL - instability line drawn in black",1);
      c_wmdrft(2,x,y);      
      c_plchhq(xp,y[0],":F22:Instability line",alsiz,0.,1.);
/*
**  intertropical front.
*/
      y[0] = y[0]-dely*0.9;
      y[1] = y[0];
      c_wmseti("T1C - one color for alternating dash pattern",2); 
      c_wmseti("T2C - second color for dash pattern",4); 
      c_wmsetc("FRO front type","tropical");
      c_wmdrft(2,x,y);      
      c_plchhq(xp,y[0],":F22:Intertropical",alsiz,0.,1.);
 
      c_frame();
 
      gdeactivate_ws (WKID);
      gclose_ws (WKID);
      gclose_gks();
}
