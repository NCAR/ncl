/*
** examples of regional weather and temperatures.
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

/*
** number of points defining region; number of regions; number of points
** in clip region.
*/
#define NU 8
#define NO 8
#define NOP1 NO+1
#define NC 5 


main()
{
/*
** data for a region.
*/
      int i,j;
      float scale;
      float xu[NU],yu[NU],xp[NU],yp[NU];
      float xoff[NO],yoff[NO];
      float xclip[NC],yclip[NC];
      char labels[NOP1][14];
      Gcolr_rep rgb;
 
      xu[0] = 0.05;
      xu[1] = 0.20;
      xu[2] = 0.40;
      xu[3] = 0.70;
      xu[4] = 0.80;
      xu[5] = 0.65;
      xu[6] = 0.40;
      xu[7] = 0.05;

      yu[0] = 0.70;
      yu[1] = 0.45;
      yu[2] = 0.60;
      yu[3] = 0.70;
      yu[4] = 0.80;
      yu[5] = 0.95;
      yu[6] = 0.84;
      yu[7] = 0.70;

      xoff[0] = 0.10;
      xoff[1] = 0.52;
      xoff[2] = 0.10;
      xoff[3] = 0.52;
      xoff[4] = 0.10;
      xoff[5] = 0.52;
      xoff[6] = 0.10;
      xoff[7] = 0.52;

      yoff[0] = 0.47;
      yoff[1] = 0.47;
      yoff[2] = 0.27;
      yoff[3] = 0.27;
      yoff[4] = 0.07;
      yoff[5] = 0.07;
      yoff[6] = -0.13;
      yoff[7] = -0.13;

      xclip[0] = 0.10;
      xclip[1] = 0.75;
      xclip[2] = 0.75;
      xclip[3] = 0.10;
      xclip[4] = 0.10;

      yclip[0] = 0.50;
      yclip[1] = 0.50;
      yclip[2] = 0.85;
      yclip[3] = 0.85;
      yclip[4] = 0.50;

      strcpy(labels[0],"Ice");
      strcpy(labels[1],"Snow");
      strcpy(labels[2],"Fluries");
      strcpy(labels[3],"Rain");
      strcpy(labels[4],"Showers");
      strcpy(labels[5],"Thunderstorms");
      strcpy(labels[6],"Temperature");
      strcpy(labels[7],"Temperature");
      strcpy(labels[8],"(clipped)");
     
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
      rgb.rgb.red = 0.; rgb.rgb.green = 1.0; rgb.rgb.blue = 0.;
      gset_colr_rep(WKID,4,&rgb);

      c_plchhq(0.50,0.93,":F26:Weather and temperature regions",0.033,0.,0.);      
      c_wmseti("COL",3);
      scale = .4;
      c_wmsetr("RHT - size scale",0.015);
      c_pcseti("FN",26);
      for(i=0; i<NO; ++i)
      {
        for(j=0; j<NU; ++j)
        {
          xp[j] = scale*xu[j]+xoff[i];
          yp[j] = scale*yu[j]+yoff[i];
        }
        if (i <= NO-3)
        {
          c_wmdrrg(NU,xp,yp,labels[i],1,xp,yp);
          c_plchhq(xp[2]+.01,yp[2]-.025,labels[i],0.02,0.,-1.);      
        }
        else if (i == NO-2)
        {
          c_wmdrrg(NU,xp,yp,"index2",1,xp,yp);
          c_plchhq(xp[2]+.01,yp[2]-.025,labels[i],0.02,0.,-1.);       
        }
        else if (i == NO-1)
        {
          for(j=0; j<NC; ++j)
          {
            xclip[j] = scale*xclip[j]+xoff[i];
            yclip[j] = scale*yclip[j]+yoff[i];
          }
          c_wmdrrg(NU,xp,yp,"index4",NC,xclip,yclip);
          c_plchhq(xp[2]+.01,yp[2]-.025,labels[i],0.02,0.,-1.);
          c_plchhq(xp[2]+.01,yp[2]-.060,labels[i+1],0.02,0.,-1.);       
        }
      }
 
      c_frame();

      gdeactivate_ws (WKID);
      gclose_ws (WKID);
      gclose_gks();
}
