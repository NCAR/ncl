/*
** examples of regional weather and temperature.
*/
#include <stdio.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>

/*
** define error file, fortran unit number, and workstation type,
** and workstation id.
*/
#define LUNIT "gmeta"
#define IWTYPE SED_WSTYPE
#define WKID 1

main()
{
/*
** open gks, open and activate a workstation.
*/
      float xe,yy;
      float xinc,yinc;
      Gcolr_rep rgb;

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
      rgb.rgb.red = 0.; rgb.rgb.green = 1.0; rgb.rgb.blue = 0.;
      gset_colr_rep(WKID,4,&rgb);
      rgb.rgb.red = 0.9; rgb.rgb.green = 0.9; rgb.rgb.blue = 0.9;
      gset_colr_rep(WKID,5,&rgb);
      rgb.rgb.red = 1.0; rgb.rgb.green = 1.0; rgb.rgb.blue = 0.;
      gset_colr_rep(WKID,6,&rgb);

      c_plchhq(0.50,0.94,":F26:Symbols and labels",0.033,0.,0.);       
/*
** his and lows
*/
      yy = 0.83;
      yinc = 0.17;
      xinc = 0.12;
      c_pcsetc("FC","%");
      c_plchhq(0.05,yy,"%F26%HIs and LOWs:",0.025,0.,-1.);       
      c_pcsetc("FC",":");
      c_pcgetr("XE",&xe);
      xe = xe+0.5*xinc;
 
      c_wmseti("HIS - shadow color for high symbols",1);
      c_wmseti("HIB - background color for high symbols",0);
      c_wmseti("HIF - character color for high symbols",1);
      c_wmseti("HIC - character color for circumscribed circle",1);
      c_wmlabs(xe,yy,"HI");
      xe = xe+xinc;
 
      c_wmsetr("SHT",0.04);
      c_wmseti("HIS - shadow color for high symbols",3);
      c_wmseti("HIB - background color for high symbols",5);
      c_wmseti("HIF - character color for high symbols",2);
      c_wmseti("HIC - character color for circumscribed circle",4);
      c_wmlabs(xe,yy,"HI");
      xe = xe+xinc;
 
      c_wmsetr("SHT",0.02);
      c_wmseti("LOS - shadow color for low symbols",5);
      c_wmseti("LOB - background color for low symbols",1);
      c_wmseti("LOF - character color for low symbols",0);
      c_wmlabs(xe,yy,"LOW");
      xe = xe+xinc;
 
      c_wmsetr("SHT",0.04);
      c_wmseti("LOS - shadow color for low symbols",2);
      c_wmseti("LOB - background color for low symbols",5);
      c_wmseti("LOF - character color for low symbols",3);
      c_wmlabs(xe,yy,"LOW");
/*
** arrows.
*/
      yy = yy-yinc;
      c_pcsetc("FC - function code character","%");
      c_plchhq(0.05,yy,"%F26%Arrows:",0.025,0.,-1.);       
      c_pcsetc("FC",":");
      c_pcgetr("XE - coordinate of end of last string",&xe);
      xe = xe+0.05;
      c_wmsetr("ARD - arrow direction",270.);
      c_wmlabs(xe,yy-0.02,"arrow");
 
      xe = xe+0.1;
      c_wmsetr("ARS - arrow size",.1);
      c_wmsetr("ARD - arrow direction",55.);
      c_wmsetr("ARL - scale factor for length of arrow tail",1.);
      c_wmseti("AWC - color index for interior of arrow",2);
      c_wmlabs(xe,yy+0.05,"arrow");
 
      xe = xe+0.04;
      gset_linewidth(2.);
      c_wmsetr("ARS - arrow size",.2);
      c_wmsetr("ARD - arrow direction",180.);
      c_wmsetr("ARL - scale factor for length of arrow tail",0.6);
      c_wmseti("AWC - color index for interior of arrow",0);
      c_wmseti("AOC - color index for arrow outline",3);
      c_wmlabs(xe,yy,"arrow");
 
      xe = xe+0.18;
      c_wmsetr("ARD - arrow direction",270.);
      c_wmseti("AWC - color index for interior of arrow",2);
      gset_linewidth(2.);
      c_wmlabs(xe,yy-0.05,"arrow");
      gset_linewidth(1.);
 
      xe = xe+0.18;
      c_wmsetr("ARD - arrow direction",0.);
      c_wmseti("AWC - color index for interior of arrow",5);
      c_wmseti("AOC - color index for arrow outline",2);
      c_wmseti("ASC - color index for arrow shadow",3);
      c_wmlabs(xe,yy,"arrow");
 
      xe = xe+0.03;
      c_wmdflt();
      c_wmsetr("ARS - arrow size",.1);
      c_wmsetr("ARD - arrow direction",180.);
      c_wmsetr("ARL - scale factor for length of arrow tail",1.8);
      c_wmlabs(xe,yy,"arrow");
/*
** dots and cities.
*/
      yy = yy-yinc;
      c_pcsetc("FC","%");
      c_plchhq(0.05,yy,"%F26%Dots and City info:",0.025,0.,-1.);       
      c_pcsetc("FC",":");
      c_pcgetr("XE",&xe);
      xe = xe+0.05;
      c_wmseti("DBC - color index for dot shadow",2);
      c_wmseti("DTC - color index for dot",3);
      c_wmsetr("DTS - size of dot",0.012);
      c_wmlabs(xe,yy,"dot");
      xe = xe+0.10;
      c_wmseti("RFC - color index for city labels",3);
      c_wmseti("CBC - color index background of city labels",0);
      c_wmsetr("CHT - size of city labels",.02);
      c_wmsetr("CMG - margins for city labels",.006);
      c_wmlabc(xe,yy,"Boulder","83/68");
      xe = xe+0.15;
      c_wmseti("DBC - color index for dot shadow",5);
      c_wmseti("DTC - color index for dot",1);
      c_wmsetr("DTS - size of dot",0.024);
      c_wmlabs(xe,yy,"dot");
      xe = xe+0.12;
      c_wmseti("RFC - color index for city labels",6);
      c_wmseti("CBC - color index background of city labels",3);
      c_wmsetr("CHT - size of city labels",.03);
      c_wmsetr("CMG - margins for city labels",.006);
      c_wmlabc(xe,yy,"Tulsa","103/83");
/*
** regional weather labels.
*/
      yy = yy-yinc;
      c_pcsetc("FC","%");
      c_plchhq(0.05,yy,"%F26%Regional labels:",0.025,0.,-1.);       
      c_pcsetc("FC",":");
      c_pcgetr("XE",&xe);
      xe = xe+0.13;
      c_wmsetr("WHT - size of label",0.02);
      c_wmlabw(xe,yy,"TORRID");
      xe = xe+0.30;
      c_wmseti("RC1 - color index for box outline",4);
      c_wmseti("RC2 - color index for character background",5);
      c_wmseti("RC3 - color index for box shadow",1);
      c_wmseti("RC4 - color index for text",3);
      c_wmseti("RC5 - color index for text outlines",2);
      c_wmsetr("WHT - size of label",0.035);
      c_wmlabw(xe,yy,"FREEZING");
/*
** regional temps.
*/
      yy = yy-yinc;
      c_pcsetc("FC","%");
      c_plchhq(0.05,yy,"%F26%Regional temps.:",0.025,0.,-1.);       
      c_pcsetc("FC",":");
      c_pcgetr("XE",&xe);
      xe = xe+0.07;
/*
** reset (primarily for arrow lengths and sizes);.
*/
      c_wmdflt();
      c_wmsetr("THT - height of regional temperature labels",0.032);
      c_wmseti("RFC - primary character color",2);
      c_wmlabt(xe,yy,"80s",0);
      xe = xe+0.12;
      c_wmsetr("ARS - arrow size",0.07);
      c_wmseti("ROS - color index for character outlines",0);
      c_wmseti("RFC - primary character color",2);
      c_wmseti("RLS - color index for shadows",1);
      c_wmlabt(xe,yy,"80s",2);
      xe = xe+0.03;
      c_wmseti("ROS - color index for character outlines",1);
      c_wmseti("RFC - primary character color",0);
      c_wmseti("RLS - color index for shadows",1);
      c_wmlabt(xe,yy,"80s",6);
      xe = xe+0.15;
      c_wmseti("ROS - color index for character outlines",-1);
      c_wmseti("RFC - primary character color",2);
      c_wmseti("RLS - color index for shadows",-1);
      c_wmseti("RBS - color index for backgrounds for labels",1);
      c_wmsetr("RMG - size of margins around characters",0.01);
      c_wmlabt(xe,yy,"80s",0);
 
      c_frame();

      gdeactivate_ws (WKID);
      gclose_ws (WKID);
      gclose_gks();
}
