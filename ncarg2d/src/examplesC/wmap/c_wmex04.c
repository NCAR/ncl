/*
** examples of parameter control of fronts.
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

#define NS 2

main()
{
      float xs[NS],ys[NS];
      float fsize;
      Gcolr_rep rgb;


      xs[0] =  0.10;
      xs[1] =  0.90; 
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
      rgb.rgb.red = 0.4; rgb.rgb.green = 0.0; rgb.rgb.blue = 0.4;
      gset_colr_rep(WKID,4,&rgb);
/*
** plot title.
*/
      c_plchhq(0.50,0.94,":F26:Parameter control of front attributes",
	         0.03,0.,0.);
/*
** various fronts with different attributes.
*/
      c_pcseti("CC",4);
      fsize = .021;
      ys[0] = .76;
      ys[1] = .76;
      c_pcsetc("FC","%");
      c_plchhq(xs[0],ys[0]+.06,
               "%F22%Starting with FRO='STA', WFC=2 (red), CFC=3 (blue):",
               fsize,0.,-1.);
      c_wmsetc("FRO","sta");
      c_wmseti("WFC",2);
      c_wmseti("CFC",3);
      c_wmdrft(NS,xs,ys);
 
      ys[0] = .60;
      ys[1] = .60;
      c_plchhq(xs[0],ys[0]+.06,
               "%F22%then setting BEG=0., END=.05, BET=.03 gives:",
               fsize,0.,-1.);
      c_wmsetr("BEG",0.00);
      c_wmsetr("END",0.05);
      c_wmsetr("BET",0.03);
      c_wmdrft(NS,xs,ys);
 
      ys[0] = .44;
      ys[1] = .44;
      c_plchhq(xs[0],ys[0]+.06,
               "%F22%then setting NMS=5 and STY=-1,2,1,-2,2 gives:",
               fsize,0.,-1.);
      c_wmseti("NMS",5);
      c_wmseti("PAI",1);
      c_wmseti("STY",-1);
      c_wmseti("PAI",2);
      c_wmseti("STY",2);
      c_wmseti("PAI",3);
      c_wmseti("STY",1);
      c_wmseti("PAI",4);
      c_wmseti("STY",-2);
      c_wmseti("PAI",5);
      c_wmseti("STY",2);
      c_wmdrft(NS,xs,ys);
 
      ys[0] = .27;
      ys[1] = .27;
      c_plchhq(xs[0],ys[0]+.07,
               "%F22%then setting SWI=.05 and LIN=12. gives:",fsize,0.,-1.);
      c_wmsetr("SWI",0.05);
      c_wmsetr("LIN",12.);
      c_wmdrft(NS,xs,ys);
 
      ys[0] = .10;
      ys[1] = .10;
      c_plchhq(xs[0],ys[0]+.07,"%F22%then setting REV=1 gives:",.022,0.,-1.);
      c_wmseti("REV",1);
      c_wmdrft(NS,xs,ys);

      c_frame();

      gdeactivate_ws (WKID);
      gclose_ws (WKID);
      gclose_gks();
}
