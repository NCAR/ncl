C
C     $Id: xy14f.f,v 1.6 2010-03-15 22:49:25 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C           Copyright (C)  1995                                        C
C   University Corporation for Atmospheric Research                    C
C           All Rights Reserved                                        C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:            xy14f.f
C
C      Author:          Fred Clare (converted to Fortran by Mary Haley)
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Tue Oct 17 10:00:12 MDT 1995
C
C Description:  This example demonstrates the data manipulation
C               capabilities of the HLUS in conjunction with XyPlot.
C
C               This example is really meant to show the data
C               manipulation capabilities of NCL, since it does some
C               array arithmetic that you can't do in C or Fortran 77.
C
      external NhlFAppClass
      external NhlFCairoWindowWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFXyPlotClass
      external NhlFCoordArraysClass
C
C Define temperature/rainfall data for a 25-hour period in
C an NCL variable.  In a real-world NCL script these values
C would probably be read in, either directly or as part of
C a data file like a netCDF or HDF file.
C
C The temperature data is hourly data in degrees Celsius and
C the rainfall data is hourly data in millimeters.
C
      real tr_data(25,2)
      data tr_data/ 9.00, 8.30, 7.40, 6.20, 5.60, 3.40, 2.70, 1.20,
     +              3.70, 5.80, 5.90, 4.50, 7.40,11.60,10.40, 9.20,
     +              7.80,10.20,12.10,11.40, 9.00, 8.90, 8.50, 7.40,
     +              6.60, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
     +              0.00, 0.00, 0.00, 0.25, 0.63, 0.10, 0.00, 0.53,
     +              3.75, 3.10, 0.52, 0.00, 0.00, 0.00, 0.00, 0.00,
     +              0.00,0.00/

      real yvalues(5)
      data yvalues/2.,1.,0.,-1.,-2./
      character*9 ylabels(5)
      data ylabels/'2~F33~s','~F33~s','~F22~MEAN','-~F33~s','-2~F33~s'/

      parameter(NHOURS=25)
      integer srlist
      real x_array(NHOURS), y_array(NHOURS)
      real x_array2(5), y_array2(5)
      real t_mean, std_dev, mm2inch
      data mm2inch/0.03937/
      integer appid, xworkid, plotid, dataid
      CHARACTER*7  wks_type
C
C Define the workstation type
C
      wks_type = "x11"
C
C
C Initialize the HLU library.
C
      call NhlFInitialize
      call NhlFRLCreate(srlist,'setrl')
C
C Create Application object.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(srlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'xy14',NhlFAppClass,0,srlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkMetaName','./xy14f.ncgm',ierr)
         call NhlFCreate(xworkid,'xy14Work',
     +        NhlFNcgmWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPause','True',ierr)
         call NhlFCreate(xworkid,'xy14Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,srlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PS workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPSFileName','./xy14f.ps',ierr)
         call NhlFCreate(xworkid,'xy14Work',
     +        NhlFPSWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPDFFileName','./xy14f.pdf',ierr)
         call NhlFCreate(xworkid,'xy14Work',
     +        NhlFPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a PS/PDF cairo workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFileName','./xy14f',ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(xworkid,'xy14Work',
     +        NhlFCairoPSPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a PNG cairo workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFileName','./xy14f',ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(xworkid,'xy14Work',
     +        NhlFCairoImageWorkstationClass,0,srlist,ierr)
      endif
C
C Create X-axis values and convert data to Fahrenheit.
C
      do 10 i = 1,NHOURS
         x_array(i) = real(i-1)
         y_array(i) = (9./5.)*tr_data(i,1)+32.
 10   continue
C
C Picture 1:
C
C Convert the temperatures to degrees Fahrenheit and plot as an XyPlot
C object in the X11 window.  Note the usage of NCL algebraic operators
C and array selection capabilities.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetFloatArray(srlist,'caXArray',x_array,NHOURS,ierr)
      call NhlFRLSetFloatArray(srlist,'caYArray',y_array,NHOURS,ierr)
      call NhlFCreate(dataid,'xyData',NhlFCoordArraysClass,appid,
     +     srlist,ierr)

      call NhlFRLClear(srlist)
      call NhlFRLSetInteger(srlist,'xyCoordData',dataid,ierr)
C
C X-axis resources
C
      call NhlFRLSetString(srlist,'tmXBMode','MANUAL',ierr)
      call NhlFRLSetFloat(srlist,'tmXBTickStartF',0.,ierr)
      call NhlFRLSetFloat(srlist,'tmXBTickEndF',24.,ierr)
      call NhlFRLSetFloat(srlist,'tmXBTickSpacingF',4.,ierr)
      call NhlFRLSetInteger(srlist,'tmXBLabelFont',22,ierr)
      call NhlFRLSetFloat(srlist,'tmXBLabelFontHeightF',0.024,ierr)
C
C Y-axis resources.  The title precision is changed to '2' so that
C integer values will be used (the temperatures are in a range
C requiring only two digits).
C
      call NhlFRLSetString(srlist,'tmYLMode','MANUAL',ierr)
      call NhlFRLSetFloat(srlist,'tmYLTickStartF',30.,ierr)
      call NhlFRLSetFloat(srlist,'tmYLTickEndF',60.,ierr)
      call NhlFRLSetFloat(srlist,'tmYLTickSpacingF',5.,ierr)
      call NhlFRLSetString(srlist,'tmYLAutoPrecision','False',ierr)
      call NhlFRLSetInteger(srlist,'tmYLPrecision',2,ierr)
      call NhlFRLSetInteger(srlist,'tmYLLabelFont',22,ierr)
      call NhlFRLSetFloat(srlist,'tmYLLabelFontHeightF',0.024,ierr)
      call NhlFRLSetInteger(srlist,'tmYLMinorPerMajor',4,ierr)
C
C Specify the Y-axis range precisely.
C
      call NhlFRLSetFloat(srlist,'trYMinF',30.,ierr)
      call NhlFRLSetFloat(srlist,'trYMaxF',60.,ierr)
C
C Supply titles
C
      call NhlFRLSetInteger(srlist,'tiMainFont',26,ierr)
      call NhlFRLSetFloat(srlist,'tiMainFontHeightF',.03,ierr)
      call NhlFRLSetString(srlist,'tiMainString','Hourly Temperatures',
     +     ierr)
      call NhlFRLSetInteger(srlist,'tiXAxisFont',22,ierr)
      call NhlFRLSetString(srlist,'tiXAxisString',
     +     'Hours Since Midnight',ierr)
      call NhlFRLSetInteger(srlist,'tiYAxisFont',22,ierr)
      call NhlFRLSetString(srlist,'tiYAxisString',
     +     'Temperature (degrees F)',ierr)
      call NhlFCreate(plotid,'XyPlot',NhlFXyPlotClass,xworkid,
     +     srlist,ierr)
      call NhlFDraw(plotid,ierr)
      call NhlFFrame(xworkid,ierr)
C
C Picture 2:
C
C Convert the rainfall data to inches and plot in the X11 window. 
C (In NCL, note how easy it is to select the rainfall data from the
C original two-dimensional data array.
C
      do 20 i = 1,NHOURS
         y_array(i) = mm2inch*tr_data(i,2)
 20   continue
      call NhlFRLClear(srlist)
      call NhlFRLSetFloatArray(srlist,'caXArray',x_array,NHOURS,ierr)
      call NhlFRLSetFloatArray(srlist,'caYArray',y_array,NHOURS,ierr)
      call NhlFCreate(dataid,'xyData',NhlFCoordArraysClass,appid,
     +     srlist,ierr)

      call NhlFRLClear(srlist)
      call NhlFRLSetInteger(srlist,'xyCoordData',dataid,ierr)
C
C X-axis resources
C
      call NhlFRLSetString(srlist,'tmXBMode','MANUAL',ierr)
      call NhlFRLSetFloat(srlist,'tmXBTickStartF',0.,ierr)
      call NhlFRLSetFloat(srlist,'tmXBTickEndF',24.,ierr)
      call NhlFRLSetFloat(srlist,'tmXBTickSpacingF',4.,ierr)
      call NhlFRLSetInteger(srlist,'tmXBLabelFont',22,ierr)
      call NhlFRLSetFloat(srlist,'tmXBLabelFontHeightF',0.024,ierr)
C
C Y-axis resources.
C
      call NhlFRLSetString(srlist,'tmYLMode','MANUAL',ierr)
      call NhlFRLSetFloat(srlist,'tmYLTickStartF',0.,ierr)
      call NhlFRLSetFloat(srlist,'tmYLTickEndF',0.15,ierr)
      call NhlFRLSetFloat(srlist,'tmYLTickSpacingF',0.03,ierr)
      call NhlFRLSetString(srlist,'tmYLAutoPrecision','False',ierr)
      call NhlFRLSetInteger(srlist,'tmYLPrecision',2,ierr)
      call NhlFRLSetInteger(srlist,'tmYLLabelFont',22,ierr)
      call NhlFRLSetFloat(srlist,'tmYLLabelFontHeightF',0.024,ierr)
      call NhlFRLSetInteger(srlist,'tmYLMinorPerMajor',4,ierr)
C
C Specify the Y-axis range precisely.
C
      call NhlFRLSetFloat(srlist,'trYMinF',0.,ierr)
      call NhlFRLSetFloat(srlist,'trYMaxF',0.15,ierr)
C
C Supply titles
C
      call NhlFRLSetInteger(srlist,'tiMainFont',26,ierr)
      call NhlFRLSetFloat(srlist,'tiMainFontHeightF',.03,ierr)
      call NhlFRLSetString(srlist,'tiMainString','Hourly Rainfall',ierr)
      call NhlFRLSetInteger(srlist,'tiXAxisFont',22,ierr)
      call NhlFRLSetString(srlist,'tiXAxisString',
     +     'Hours Since Midnight',ierr)
      call NhlFRLSetInteger(srlist,'tiYAxisFont',22,ierr)
      call NhlFRLSetString(srlist,'tiYAxisString','Rainfall in Inches',
     +     ierr)
      call NhlFCreate(plotid,'XyPlot',NhlFXyPlotClass,xworkid,
     +     srlist,ierr)
      call NhlFDraw(plotid,ierr)
      call NhlFFrame(xworkid,ierr)
C
C Picture 3:
C
C For each hourly temperature reading, plot the number of standard
C deviations from the mean the reading represents.  (In NCL, note the
C use of the NCL exponentiation operator (^) and the sqrt function.)
C
      t_mean = 0.
      do 30 i = 1,NHOURS
         t_mean = t_mean+(9./5.)*tr_data(i,1)+32.
 30   continue
      t_mean = t_mean/NHOURS
      std_dev = 0.
      do 40 i=1,NHOURS
         std_dev = std_dev+(((9./5.)*tr_data(i,1)+32.) - t_mean)**2.
 40   continue
      std_dev = sqrt(std_dev/(NHOURS-1))
      do 50 i = 1,NHOURS
         y_array(i) = ((9./5.)*tr_data(i,1)+32. - t_mean)/std_dev
 50   continue
      call NhlFRLClear(srlist)
      call NhlFRLSetFloatArray(srlist,'caXArray',x_array,NHOURS,ierr)
      call NhlFRLSetFloatArray(srlist,'caYArray',y_array,NHOURS,ierr)
      call NhlFCreate(dataid,'xyData',NhlFcoordArraysClass,appid,srlist,
     +     ierr)
      
      call NhlFRLClear(srlist)
C
C X-axis resources
C
      call NhlFRLSetString(srlist,'tmXBMode','MANUAL',ierr)
      call NhlFRLSetFloat(srlist,'tmXBTickStartF',0.,ierr)
      call NhlFRLSetFloat(srlist,'tmXBTickEndF',24.,ierr)
      call NhlFRLSetFloat(srlist,'tmXBTickSpacingF',4.,ierr)
      call NhlFRLSetInteger(srlist,'tmXBLabelFont',22,ierr)
      call NhlFRLSetFloat(srlist,'tmXBLabelFontHeightF',0.024,ierr)
C
C Y-axis resources.
C
      call NhlFRLSetString(srlist,'tmYLMode','EXPLICIT',ierr)
      call NhlFRLSetFloatArray(srlist,'tmYLValues',yvalues,5,ierr)
      call NhlFRLSetStringArray(srlist,'tmYLLabels',ylabels,5,ierr)
      call NhlFRLSetFloat(srlist,'tmYLTickSpacingF',1.,ierr)
      call NhlFRLSetInteger(srlist,'tmYLLabelFont',22,ierr)
      call NhlFRLSetFloat(srlist,'tmYLLabelFontHeightF',0.024,ierr)
      call NhlFRLSetString(srlist,'tmYMajorGrid','True',ierr)
      call NhlFRLSetString(srlist,'tmYLMinorOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYRMinorOn','False',ierr)
C
C Specify the Y-axis range precisely.
C
      call NhlFRLSetFloat(srlist,'trYMinF',-2.5,ierr)
      call NhlFRLSetFloat(srlist,'trYMaxF',2.5,ierr)
C
C Supply titles
C
      call NhlFRLSetInteger(srlist,'tiMainFont',26,ierr)
      call NhlFRLSetFloat(srlist,'tiMainFontHeightF',.03,ierr)
      call NhlFRLSetString(srlist,'tiMainString',
     +     'Temperature Deviations From Mean',ierr)
      call NhlFRLSetInteger(srlist,'tiXAxisFont',22,ierr)
      call NhlFRLSetString(srlist,'tiXAxisString',
     +     'Hours Since Midnight',ierr)
      call NhlFRLSetString(srlist,'tiYAxisString','Deviations',ierr)
      call NhlFRLSetInteger(srlist,'tiYAxisFont',22,ierr)
      call NhlFCreate(plotid,'XyPlot',NhlFxyPlotClass,xworkid,srlist,
     +     ierr)
C
C Specify the use of filled circle markers.
C
      call NhlFAddData(plotid,'xyCoordData',dataid,dataspec)

      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'xyMarkLineMode','MARKERS',ierr)
      call NhlFRLSetString(srlist,'xyMonoMarkLineMode','True',ierr)
      call NhlFRLSetInteger(srlist,'xyMarker',16,ierr)
      call NhlFRLSetString(srlist,'xyMonoMarkerSize','True',ierr)
      call NhlFRLSetFloat(srlist,'xyMarkerSizeF',.02,ierr)
      call NhlFSetValues(dataspec,srlist,ierr)
      call NhlFDraw(plotid,ierr)
      call NhlFFrame(xworkid,ierr)
C
C Picture 4:
C (In Ncl, this section illustrate the use of being able to easily pick
C out subsections of data arrays.
C
      j = 1
      do 60 i = 3,NHOURS,6
         x_array2(j) = x_array(i)
         y_array2(j) = (9./5.)*tr_data(i,1)+32.
         j = j+1
 60   continue
      call NhlFRLClear(srlist)
      call NhlFRLSetFloatArray(srlist,'caXArray',x_array2,j-1,ierr)
      call NhlFRLSetFloatArray(srlist,'caYArray',y_array2,j-1,ierr)
      call NhlFCreate(dataid,'xyData',NhlFcoordArraysClass,appid,srlist,
     +     ierr)
      call NhlFRLClear(srlist)
      call NhlFRLSetInteger(srlist,'xyCoordData',dataid,ierr)
C
C X-axis resources 
C
      call NhlFRLSetString(srlist,'tmXBMode','MANUAL',ierr)
      call NhlFRLSetFloat(srlist,'tmXBTickStartF',2.,ierr)
      call NhlFRLSetFloat(srlist,'tmXBTickEndF',22.,ierr)
      call NhlFRLSetFloat(srlist,'tmXBTickSpacingF',6.,ierr)
      call NhlFRLSetInteger(srlist,'tmXBLabelFont',22,ierr)
      call NhlFRLSetFloat(srlist,'tmXBLabelFontHeightF',0.024,ierr)
      call NhlFRLSetInteger(srlist,'tmXBMinorPerMajor',0,ierr)
C
C Y-axis resources.
C
      call NhlFRLSetString(srlist,'tmYLMode','MANUAL',ierr)
      call NhlFRLSetFloat(srlist,'tmYLTickStartF',30.,ierr)
      call NhlFRLSetFloat(srlist,'tmYLTickEndF',60.,ierr)
      call NhlFRLSetFloat(srlist,'tmYLTickSpacingF',5.,ierr)
      call NhlFRLSetString(srlist,'tmYLAutoPrecision','False',ierr)
      call NhlFRLSetInteger(srlist,'tmYLPrecision',2,ierr)
      call NhlFRLSetInteger(srlist,'tmYLLabelFont',22,ierr)
      call NhlFRLSetFloat(srlist,'tmYLLabelFontHeightF',0.024,ierr)
      call NhlFRLSetInteger(srlist,'tmYLMinorPerMajor',4,ierr)
C
C Specify the Y-axis range precisely.
C
      call NhlFRLSetFloat(srlist,'trYMinF',30.,ierr)
      call NhlFRLSetFloat(srlist,'trYMaxF',60.,ierr)
C
C Supply titles
C
      call NhlFRLSetInteger(srlist,'tiMainFont',26,ierr)
      call NhlFRLSetString(srlist,'tiMainString',
     +     'Temperatures at Six Hour Intervals',ierr)
      call NhlFRLSetFloat(srlist,'tiMainFontHeightF',.03,ierr)
      call NhlFRLSetInteger(srlist,'tiXAxisFont',22,ierr)
      call NhlFRLSetString(srlist,'tiXAxisString',
     +     'Hours Since Midnight',ierr)
      call NhlFRLSetInteger(srlist,'tiYAxisFont',22,ierr)
      call NhlFRLSetString(srlist,'tiYAxisString',
     +     'Temperature (degrees F)',ierr)
      call NhlFCreate(plotid,'XyPlot',NhlFxyPlotClass,xworkid,srlist,
     +     ierr)

      call NhlFDraw(plotid,ierr)
      call NhlFFrame(xworkid,ierr)
C
C NhlDestroy destroys the given id and all of its children.
C
      call NhlFDestroy(appid,ierr)
      call NhlFRLDestroy(srlist)
C
C Restores state.
C
      call NhlFClose

      end
