/*
** examples of controlling slopes at endpoints of fronts.
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

#define NV 5 

main()
{
      int i,j;
      char label[28];
      Gcolr_rep rgb;

/*
** data for picture two illustrating slope control at end points.
*/
      float xv[NV],yv[NV];

      xv[0] =  0.10;
      xv[1] =  0.30;
      xv[2] =  0.50;
      xv[3] =  0.70;
      xv[4] =  0.90;

      yv[0] =  1.00;
      yv[1] =  1.08;
      yv[2] =  1.00;
      yv[3] =  0.95;
      yv[4] =  0.94;
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

      c_pcseti("cc",1);
      c_plchhq(0.50,0.96,":F26:Slope control at endpoints",0.03,0.,0.);
/*
** set some parameter values.
*/
      c_wmseti("NMS - number of symbols on front line",6);
      c_wmsetr("SL1 - slope at start of front line if slf=0 or 1",0.);       
      c_wmsetr("SL2 - slope at end of front line if slf=0 or 1",-15.);       
      c_wmseti("WFC - color for warm fronts",1);
      c_wmsetr("LIN - line widths of front lines",3.);
      c_wmseti("REV - line widths of front lines",1);
      c_wmseti("WFC - color for warm fronts",2);
      c_pcseti("CC",3);

      for(i=3; i>=0; --i)
      {
        c_wmseti("SLF - flags whether slopes are from sl1 and sl2",i);

        for(j=0; j<NV; ++j)
        {
          yv[j] = yv[j]-0.22;
        }

        c_wmdrft(NV,xv,yv);
        sprintf(label,":F22:SLF=%d, SL1=0., SL2=-15.",i);
        c_plchhq(.7,yv[0]+.08,label,.024,0.,0.);
      }

      c_frame();

      gdeactivate_ws (WKID);
      gclose_ws (WKID);
      gclose_gks();
}
