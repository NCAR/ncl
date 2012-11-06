/*
 *      $Id: xy17c.c,v 1.9 2010-03-15 22:49:25 haley Exp $
 */
/***********************************************************************
 *                                                                     *
 *                Copyright (C)  1996                                  *
 *        University Corporation for Atmospheric Research              *
 *                All Rights Reserved                                  *
 *                                                                     *
 ***********************************************************************
 *
 *  File:       xy17c.c
 *
 *  Author:     Bob Lackman
 *              National Center for Atmospheric Research
 *              PO 3000, Boulder, Colorado
 *
 *  Converted to C by : Scott Snodgrass
 *
 *  Date:       14 Mar 1997
 *
 *  Description:    Reads an ASCII file with 4 variables:
 *                  lon, u, v, and t.  u, v, and t are plotted
 *                  with 3 stacked y axes.
 *
 */

#include <stdio.h>
#include <string.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/XyPlot.h>
#include <ncarg/hlu/CoordArrays.h>

#define ncurve 3
#define npts 129

int main ()
{
   char const *wks_type = "x11";
   int i=0, rlist, wks, appid, field1, field2, field3, xy1, xy2, xy3;
   int grlist, datadepid[1];
   int *dspec = datadepid;
   ng_size_t num_dspec;

/*
 *  Create variables to contain data.
 */

   FILE *x1_y3;
   float y [ncurve][npts], lon [npts], u [npts], v [npts], t [npts];
   float y1val [5] = {-90.0 , -80.0, -70.0, -60.0, -50.0};
   float y2val [6] = {10.0, 20.0, 30.0, 40.0, 50.0, 60.0};
   float y3val [5] = {-20.0, -10.0, 0.0, 10.0, 20.0};
 
   char *y1lab [5] = {"-90.", "-80.", "-70.", "-60.", "-50."};
   char *y2lab [6] = {"10.", "20.", "30.", "40.", "50.", "60."};
   char *y3lab [5] = {"-20.", "-10.", "0.", "10.", "20."};
   char *file = "xy.asc";

/*
 *  Read ASCII file xy.asc
 */
   x1_y3 = fopen (file,"r");
/*
 *   xy.asc has 4 vars of length 129 longitudes, lon, u, v, t
 *
 *     The data is taken at 43N latitude.  Longitude is an index
 *     1-129 standing for 0 deg - 360 deg in steps of 360/128?
 *     u and v are in m/s, and t is in deg K.
 */

   while (!feof(x1_y3)) {
      fscanf (x1_y3, "%f %f %f %f", &lon[i], &u[i], &v[i], &t[i]);
      i++;
   }

   for (i=0; i < npts; i++)
   {
      lon [i] = (lon[i]- 1.0) * 360.0/128.0;
      t [i] =  (t[i] - 273.15) * 9 / 5 + 32.0 ;
      y [0][i] = u[i];
      y [1][i] = v[i];
      y [2][i] = (t[i] - 273.15) * 9.0 / 5.0 + 32.0;
   }

   NhlInitialize ();
   rlist = NhlRLCreate (NhlSETRL);

/*
 *  Create Application object.  The Application object name is used to
 *  determine the name of the resource file, which is "xy17.res" in this
 *  case. 
 */
 
   NhlRLClear (rlist);
   NhlRLSetInteger (rlist, NhlNappDefaultParent, True);
   NhlRLSetString (rlist, NhlNappUsrDir, "./");
   NhlCreate (&appid, "xy17", NhlappClass, 0, rlist);

/*
 *  If NCGM=1, then open NCGM workstation. 
 */
   if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
      NhlRLClear (rlist);
      NhlRLSetString (rlist, NhlNwkMetaName, "xy17c.ncgm");
      NhlCreate (&wks, "xy17Work", NhlncgmWorkstationClass, 0, rlist);
   }
   else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 *  Create an X workstation. 
 */
      NhlRLClear (rlist);
      NhlRLSetInteger (rlist, NhlNwkPause, True);
      NhlCreate (&wks, "xy17Work", NhlcairoWindowWorkstationClass, 0, rlist);
   }
   else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 *  Open PS workstation. 
 */

      NhlRLClear (rlist);
      NhlRLSetString (rlist, NhlNwkPSFileName, "xy17c.ps");
      NhlCreate (&wks, "xy17Work", NhlpsWorkstationClass, 0, rlist);
   }
   else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 *  Open PDF workstation. 
 */
      NhlRLClear (rlist);
      NhlRLSetString (rlist, NhlNwkPDFFileName, "xy17c.pdf");
      NhlCreate (&wks, "xy17Work", NhlpdfWorkstationClass, 0, rlist);
   }
   else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
            !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 *  Open cairo PS/PDF workstation. 
 */
      NhlRLClear (rlist);
      NhlRLSetString (rlist, NhlNwkFileName, "xy17c");
      NhlRLSetString (rlist, NhlNwkFormat, (char*)wks_type);
      NhlCreate (&wks, "xy17Work", NhlcairoDocumentWorkstationClass, 0, rlist);
   }
   else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 *  Open cairo PNG workstation. 
 */
      NhlRLClear (rlist);
      NhlRLSetString (rlist, NhlNwkFileName, "xy17c");
      NhlRLSetString (rlist, NhlNwkFormat, (char*)wks_type);
      NhlCreate (&wks, "xy17Work", NhlcairoImageWorkstationClass, 0, rlist);
   }

/*
 *  Create a coordarrays data object and configure its extents missing
 *  values and at the same time convert it from Degrees K to Degrees F  
 */

   NhlRLClear (rlist);
   NhlRLSetFloatArray (rlist, NhlNcaXArray, lon, 129);
   NhlRLSetFloatArray (rlist, NhlNcaYArray, t, 129);
   NhlCreate (&field1, "field1", NhlcoordArraysClass, appid, rlist);

/*
 *  Create a coordarrays data object and configure its extents missing
 *  values and at the same time convert it from Degrees K to Degrees F
 */

   NhlRLClear (rlist);
   NhlRLSetFloatArray (rlist, NhlNcaXArray, lon, 129);
   NhlRLSetFloatArray (rlist, NhlNcaYArray, u, 129);
   NhlCreate (&field2, "field2", NhlcoordArraysClass, appid, rlist);

/*
 *  Create a coordarrays data object and configure its extents missing
 *  values and at the same time convert it from Degrees K to Degrees F
 */

   NhlRLClear (rlist);
   NhlRLSetFloatArray (rlist, NhlNcaXArray, lon, 129);
   NhlRLSetFloatArray (rlist, NhlNcaYArray, v, 129);
   NhlCreate (&field3, "field3", NhlcoordArraysClass, appid, rlist);

/*
 *  Create XyPlot object for curve 1 and assign data to it
 */

   NhlRLClear (rlist);
   NhlRLSetFloat   (rlist, NhlNvpXF, 0.20);
   NhlRLSetFloat   (rlist, NhlNvpYF, 0.80);
   NhlRLSetFloat   (rlist, NhlNvpWidthF, 0.6);
   NhlRLSetFloat   (rlist, NhlNvpHeightF, 0.2);
   NhlRLSetInteger (rlist, NhlNxyCoordData, field1);
   NhlRLSetString  (rlist, NhlNtrYReverse, "False");
   NhlRLSetFloat   (rlist, NhlNtrYMaxF,  -50.0);
   NhlRLSetFloat   (rlist, NhlNtrYMinF,  -90.0);
   NhlRLSetFloat   (rlist, NhlNtrXMaxF,  360.0);
   NhlRLSetFloat   (rlist, NhlNtrXMinF,   0.0);
   NhlRLSetString  (rlist, NhlNtmYROn, "False");
   NhlRLSetString  (rlist, NhlNtmYUseLeft, "False");
   NhlRLSetString  (rlist, NhlNtmXMajorGrid, "True");
   NhlRLSetString  (rlist, NhlNtmXBLabelsOn, "False");
   NhlRLSetString  (rlist, NhlNtmYLLabelsOn, "True");
   NhlRLSetFloat   (rlist, NhlNtmYLMajorLengthF, 0.01);
   NhlRLSetFloat   (rlist, NhlNtmYLMajorOutwardLengthF, 0.0);
   NhlRLSetString  (rlist, NhlNtmYLMode, "Explicit");
   NhlRLSetFloatArray  (rlist, NhlNtmYLValues, y1val, 5);
   NhlRLSetStringArray (rlist, NhlNtmYLLabels, y1lab, 5);
   NhlRLSetString  (rlist, NhlNtmYLLabelsOn, "True");
   NhlRLSetString (rlist, NhlNtmYLLabelFontColor,"red");
   NhlRLSetString  (rlist, NhlNtiMainString, "Temperature, U, V Stacked Plots");
   NhlRLSetString  (rlist, NhlNtiYAxisString, "Temp (Deg C)");
   NhlRLSetFloat   (rlist, NhlNtiXAxisFontHeightF, 0.02);
   NhlRLSetFloat   (rlist, NhlNtiYAxisFontHeightF, 0.02);
   NhlRLSetString  (rlist, NhlNtiXAxisFont, "helvetica-bold");
   NhlRLSetString  (rlist, NhlNtiYAxisFont, "helvetica-bold");
   NhlRLSetString (rlist, NhlNtiYAxisFontColor,"red");
   NhlRLSetString  (rlist, NhlNtmYRMinorOn, "False");
   NhlRLSetString  (rlist, NhlNtmYLMinorOn, "False");
   NhlCreate(&xy1, "xy1", NhlxyPlotClass, wks, rlist);

/*
 *  Create XyPlot object for curve 2 and assign data to it
 */

   NhlRLClear (rlist);
   NhlRLSetFloat   (rlist, NhlNvpXF, 0.20);
   NhlRLSetFloat   (rlist, NhlNvpYF, 0.60);
   NhlRLSetFloat   (rlist, NhlNvpWidthF, 0.6);
   NhlRLSetFloat   (rlist, NhlNvpHeightF, 0.2);
   NhlRLSetInteger (rlist, NhlNxyCoordData, field2);
   NhlRLSetString  (rlist, NhlNtrYReverse, "False");
   NhlRLSetFloat   (rlist, NhlNtrYMaxF,   60.0);
   NhlRLSetFloat   (rlist, NhlNtrYMinF,   10.0);
   NhlRLSetFloat   (rlist, NhlNtrXMaxF,  360.0);
   NhlRLSetFloat   (rlist, NhlNtrXMinF,   0.0);
   NhlRLSetString  (rlist, NhlNtmYROn, "True");
   NhlRLSetString  (rlist, NhlNtmYLOn, "False");
   NhlRLSetString  (rlist, NhlNtmYUseLeft, "False");
   NhlRLSetString  (rlist, NhlNtmXMajorGrid, "True");
   NhlRLSetString  (rlist, NhlNtmYLLabelsOn, "False");
   NhlRLSetString  (rlist, NhlNtmYRLabelsOn, "True");
   NhlRLSetString  (rlist, NhlNtmYRMode, "Explicit");
   NhlRLSetFloatArray  (rlist, NhlNtmYRValues, y2val, 6);
   NhlRLSetStringArray (rlist, NhlNtmYRLabels, y2lab, 6);
   NhlRLSetString  (rlist, NhlNtmXBLabelsOn, "False");
   NhlRLSetString (rlist, NhlNtmYRLabelFontColor,"green");
   NhlRLSetString  (rlist, NhlNtiYAxisString, "U (m/s)");
   NhlRLSetFloat   (rlist, NhlNtiXAxisFontHeightF, 0.02);
   NhlRLSetFloat   (rlist, NhlNtiYAxisFontHeightF, 0.02);
   NhlRLSetString  (rlist, NhlNtiXAxisFont, "helvetica-bold");
   NhlRLSetString  (rlist, NhlNtiYAxisFont, "helvetica-bold");
   NhlRLSetString (rlist, NhlNtiYAxisFontColor,"green");
   NhlRLSetString  (rlist, NhlNtmYRMinorOn, "False");
   NhlRLSetString  (rlist, NhlNtmYLMinorOn, "False");
   NhlCreate(&xy2, "xy2", NhlxyPlotClass, wks, rlist);

/*
 *  Create XyPlot object for curve 3 and assign data to it
 *
 *  Increase the veiwport so the right scale will be about .15 NDC
 *  right of the other grids.  Plot only the right vertical axis.
 *  .5NDC = 360 deg lon, thus .65NDC = 360+108 deg lon.
 */

   NhlRLClear (rlist);
   NhlRLSetFloat   (rlist, NhlNvpXF, 0.20);
   NhlRLSetFloat   (rlist, NhlNvpYF, 0.40);
   NhlRLSetFloat   (rlist, NhlNvpWidthF, 0.6);
   NhlRLSetFloat   (rlist, NhlNvpHeightF, 0.2);
   NhlRLSetInteger (rlist, NhlNxyCoordData, field3);
   NhlRLSetString  (rlist, NhlNtrYReverse, "False");
   NhlRLSetFloat   (rlist, NhlNtrYMaxF,   20.0);
   NhlRLSetFloat   (rlist, NhlNtrYMinF,  -20.0);
   NhlRLSetFloat   (rlist, NhlNtrXMaxF,  360.0);
   NhlRLSetFloat   (rlist, NhlNtrXMinF,   0.0);
   NhlRLSetString  (rlist, NhlNtmYROn, "False");
   NhlRLSetString  (rlist, NhlNtmYUseLeft, "False");
   NhlRLSetString  (rlist, NhlNtmYLLabelsOn, "True");
   NhlRLSetString  (rlist, NhlNtmXBLabelsOn, "True");
   NhlRLSetString  (rlist, NhlNtmXMajorGrid, "True");
   NhlRLSetFloat   (rlist, NhlNtmYLMajorLengthF, 0.01);
   NhlRLSetFloat   (rlist, NhlNtmYLMajorOutwardLengthF, 0.0);
   NhlRLSetString  (rlist, NhlNtmYLMode, "Explicit");
   NhlRLSetString  (rlist, NhlNtmYLLabelsOn, "True");
   NhlRLSetString (rlist, NhlNtmYLLabelFontColor,"blue");
   NhlRLSetString  (rlist, NhlNtiYAxisString, "V (m/s)");
   NhlRLSetString  (rlist, NhlNtiXAxisString, "Longitude (Degs)");
   NhlRLSetFloat   (rlist, NhlNtiXAxisFontHeightF, 0.02);
   NhlRLSetFloat   (rlist, NhlNtiYAxisFontHeightF, 0.02);
   NhlRLSetString  (rlist, NhlNtiXAxisFont, "helvetica-bold");
   NhlRLSetString  (rlist, NhlNtiYAxisFont, "helvetica-bold");
   NhlRLSetString (rlist, NhlNtiYAxisFontColor,"blue");
   NhlRLSetString  (rlist, NhlNtmYRMinorOn, "False");
   NhlRLSetString  (rlist, NhlNtmYLMinorOn, "False");
   NhlRLSetFloatArray  (rlist, NhlNtmYLValues, y3val, 5);
   NhlRLSetStringArray (rlist, NhlNtmYLLabels, y3lab, 5);
   NhlCreate(&xy3, "xy3", NhlxyPlotClass, wks, rlist);

   grlist = NhlRLCreate (NhlGETRL);
   NhlRLClear (grlist);
   NhlRLGetIntegerArray(grlist,NhlNxyCoordDataSpec,&dspec,&num_dspec);
   NhlGetValues(xy1, grlist);

   NhlRLClear (rlist);
   NhlRLSetInteger (rlist, NhlNxyMonoLineColor, True);
   NhlRLSetString (rlist, NhlNxyLineColor,"red");
   NhlSetValues (dspec[0], rlist);

   NhlRLClear (grlist);
   NhlRLGetIntegerArray(grlist,NhlNxyCoordDataSpec,&dspec,&num_dspec);
   NhlGetValues(xy2, grlist);

   NhlRLClear (rlist);
   NhlRLSetInteger (rlist, NhlNxyMonoLineColor, True);
   NhlRLSetString (rlist, NhlNxyLineColor,"green");
   NhlSetValues (dspec[0], rlist);

   NhlRLClear (grlist);
   NhlRLGetIntegerArray(grlist,NhlNxyCoordDataSpec,&dspec,&num_dspec);
   NhlGetValues(xy3, grlist);

   NhlRLClear (rlist);
   NhlRLSetInteger (rlist, NhlNxyMonoLineColor, True);
   NhlRLSetString (rlist, NhlNxyLineColor,"blue");
   NhlSetValues (dspec[0], rlist);

   NhlDraw(xy1);
   NhlDraw(xy2);
   NhlDraw(xy3);
   NhlFrame(wks);

   NhlDestroy (wks);
   NhlClose();
   exit(0);
}
