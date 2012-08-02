C
C      $Id: xy16f.f,v 1.8 2010-03-15 22:49:25 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                       C
C                Copyright (C)  1996                                    C
C        University Corporation for Atmospheric Research                C
C                All Rights Reserved                                    C
C                                                                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   File:       xy16f.f
C
C   Author:     Bob Lackman
C           National Center for Atmospheric Research
C           PO 3000, Boulder, Colorado
C
C   Date:       24 Jan 1996
C
C   Description:    Reads an ASCII file with 4 variables:
C                   lon, u, v, and t.  u, v, and t are plotted
C                   with 3 separate y axes.
C
C                   This example shows how the Data Spec object
C                   can be retrieved so you can change certain
C                   aspects of the plot, like the line color.
C                   line color.
C
C                   In this example, each of the three variables
C                   are assigned their own Y axis and scales by
C                   creating three distinct XyPlot objects. The
C                   first object is given a full grid and has
C                   its Y axis scale on the left. The second 
C                   object has no grid and the Y axis scale on
C                   the right of the first grid. The third
C                   object's viewport is made wider than the
C                   first two and its X axis is scaled so that
C                   the data end at the right grid boundary of
C                   the first object.  Only the Y axis on the
C                   right is drawn for the third object. The
C                   curves and the Y axis scales are color
C                   coordinated so you can tell which curve
C                   goes with which scale.
C                   
C                   There is no resource file for this example.
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFCoordArraysClass
      external NhlFXyPlotClass
      external NhlFtitleclass
      external NhlFTextItemClass
      external NhlFTickMarkClass
      parameter(NCURVE=3,NPTS=129)
C
C Define variables for data
C
      real lon(NPTS),u(NPTS),v(NPTS),t(NPTS)
C
C Define variables for tick mark labels
C
      real ylvals(5),yrvals1(6),yrvals2(5)
      character yllabs(5)*4,yrlabs1(6)*3,yrlabs2(5)*4
      data ylvals/-90.,-80.,-70.,-60.,-50./
      data yllabs/'-90.','-80.','-70.','-60.','-50.'/
      data yrvals1/0.,10.,20.,30.,40.,50./
      data yrlabs1/'0.','10.','20.','30.','40.','50.'/
      data yrvals2/-20.,-10.,0.,10.,20./
      data yrlabs2/'-20.','-10.','0.','10.','20.'/
C
C Define HLU variables for creating objects.
C
      integer appid,field1,field2,field3,xy1,xy2,xy3,xworkid
      integer grlist, srlist, spec1, spec2, spec3
C
C Define variable for data file
C
      character*6 filename
      data filename/'xy.asc'/
C
C Indicate whether we want output to go to NCGM, X11 window or
C PS file.
C
      CHARACTER*7  wks_type
C
C Define the workstation type
C
      wks_type = "x11"
C
C Create Application object.
C
      call NhlFInitialize
      call NhlFRLCreate(srlist,'setrl')
      call NhlFRLCreate(grlist,'getrl')
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(srlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'xy16',NhlFAppClass,0,srlist,ierr)
C
C Read ASCII file "xy.asc".
C
      open(unit=10,file=filename,status='old',form='formatted',err=104)
C
C Read data.
C
      read(10,*)(lon(i),u(i),v(i),t(i),i=1,NPTS)
C
C Create an NCGM workstation.
C
      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkMetaName','./xy16f.ncgm',ierr)
         call NhlFCreate(xworkid,'xy16Work',
     +        NhlFNcgmWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPause','True',ierr)
         call NhlFCreate(xworkid,'xy16Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,srlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPSFileName','./xy16f.ps',ierr)
         call NhlFCreate(xworkid,'xy16Work',
     +        NhlFPSWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPDFFileName','./xy16f.pdf',ierr)
         call NhlFCreate(xworkid,'xy16Work',
     +        NhlFPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFileName','./xy16f',ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(xworkid,'xy16Work',
     +        NhlFCairoPSPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFileName','./xy16f',ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(xworkid,'xy16Work',
     +        NhlFCairoImageWorkstationClass,0,srlist,ierr)
      endif
C
C  xy.asc has 4 vars of length 129 longitudes, lon, u, v, t
C
C     The data is taken at 43N latitude.  Longitude is an index
C     1-129 standing for 0 deg - 360 deg in steps of 360/128?
C     u and v are in m/s, and t is in deg K.
C
C     Convert from degrees K to degrees F and configure its extents
C     missing values.
C
      do i=1,NPTS
         t(i) = (t(i) - 273.15) * 9 / 5 + 32.0
         lon(i) = (lon(i) - 1.) * 360./128.
      end do
C
C Create the first Coord Arrays data object.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetFloatArray(srlist,'caYArray',t,NPTS,ierr)
      call NhlFRLSetFloatArray(srlist,'caXArray',lon,NPTS,ierr)
      call NhlFCreate(field1,'field1',NhlFCoordArraysClass,
     +                0,srlist,ierr)
C
C Create the second Coord Arrays data object.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetFloatArray(srlist,'caYArray',u,NPTS,ierr)
      call NhlFRLSetFloatArray(srlist,'caXArray',lon,NPTS,ierr)
      call NhlFCreate(field2,'field2',NhlFCoordArraysClass,
     +                0,srlist,ierr)
C
C Create the third Coord Arrays data object.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetFloatArray(srlist,'caYArray',v,NPTS,ierr)
      call NhlFRLSetFloatArray(srlist,'caXArray',lon,NPTS,ierr)
      call NhlFCreate(field3,'field3',NhlFCoordArraysClass,
     +                0,srlist,ierr)
C
C Create XyPlot object for curve 1 and assign data to it.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetFloat(srlist,'vpXF',.20,ierr)
      call NhlFRLSetFloat(srlist,'vpYF',.80,ierr)
      call NhlFRLSetFloat(srlist,'vpWidthF',.5,ierr)
      call NhlFRLSetFloat(srlist,'vpHeightF',.6,ierr)
      call NhlFRLSetInteger(srlist,'xyCoordData',field1,ierr)
      call NhlFRLSetString(srlist,'trYReverse','False',ierr)
      call NhlFRLSetFloat(srlist,'trYMaxF',-50.,ierr)
      call NhlFRLSetFloat(srlist,'trYMinF',-90.,ierr)
      call NhlFRLSetFloat(srlist,'trXMaxF',360.,ierr)
      call NhlFRLSetFloat(srlist,'trXMinF',0.,ierr)
      call NhlFRLSetString(srlist,'tmYROn','False',ierr)
      call NhlFRLSetString(srlist,'tmYUseLeft','False',ierr)
      call NhlFRLSetString(srlist,'tmYLLabelsOn','True',ierr)
      call NhlFRLSetFloat(srlist,'tmYLMajorLengthF',.01,ierr)
      call NhlFRLSetFloat(srlist,'tmYLMajorOutwardLengthF',.01,ierr)
      call NhlFRLSetString(srlist,'tmYLMode','Explicit',ierr)
      call NhlFRLSetFloatArray(srlist,'tmYLValues',ylvals,5,ierr)
      call NhlFRLSetStringArray(srlist,'tmYLLabels',yllabs,5,ierr)
      call NhlFRLSetString(srlist,'tmYLLabelsOn','True',ierr)
      call NhlFRLSetString(srlist,'tmYLLabelFontColor','red',ierr)
      call NhlFRLSetString(srlist,'tiXAxisString','Longitude (Degs)',
     +     ierr)
      call NhlFRLSetString(srlist,'tiYAxisString',
     +     'Temperature in Deg C',ierr)
      call NhlFRLSetFloat(srlist,'tiXAxisFontHeightF',0.02,ierr)
      call NhlFRLSetFloat(srlist,'tiYAxisFontHeightF',0.02,ierr)
      call NhlFRLSetString(srlist,'tiXAxisFont','helvetica-bold',ierr)
      call NhlFRLSetString(srlist,'tiYAxisFont','helvetica-bold',ierr)
      call NhlFRLSetString(srlist,'tiYAxisFontColor','red',ierr)
      call NhlFRLSetString(srlist,'tmYRMinorOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYLMinorOn','False',ierr)
      call NhlFCreate(xy1,'xy1',NhlFXyPlotClass,xworkid,
     +     srlist,ierr)
C
C Create XyPlot object for curve 2 and assign data to it.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetFloat(srlist,'vpXF',.20,ierr)
      call NhlFRLSetFloat(srlist,'vpYF',.80,ierr)
      call NhlFRLSetFloat(srlist,'vpWidthF',.5,ierr)
      call NhlFRLSetFloat(srlist,'vpHeightF',.6,ierr)
      call NhlFRLSetInteger(srlist,'xyCoordData',field2,ierr)
      call NhlFRLSetString(srlist,'trYReverse','False',ierr)
      call NhlFRLSetFloat(srlist,'trYMaxF',50.,ierr)
      call NhlFRLSetFloat(srlist,'trYMinF',0.,ierr)
      call NhlFRLSetFloat(srlist,'trXMaxF',360.,ierr)
      call NhlFRLSetFloat(srlist,'trXMinF',0.,ierr)
      call NhlFRLSetString(srlist,'tmYROn','True',ierr)
      call NhlFRLSetString(srlist,'tmYLOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYUseLeft','False',ierr)
      call NhlFRLSetString(srlist,'tmYRLabelsOn','True',ierr)
      call NhlFRLSetString(srlist,'tmYLLabelsOn','False',ierr)
      call NhlFRLSetFloat(srlist,'tmYRMajorLengthF',.01,ierr)
      call NhlFRLSetFloat(srlist,'tmYRMajorOutwardLengthF',.01,ierr)
      call NhlFRLSetString(srlist,'tmYRMode','Explicit',ierr)
      call NhlFRLSetFloatArray(srlist,'tmYRValues',yrvals1,6,ierr)
      call NhlFRLSetStringArray(srlist,'tmYRLabels',yrlabs1,6,ierr)
      call NhlFRLSetString(srlist,'tmYRLabelFontColor','cyan',ierr)
      call NhlFRLSetString(srlist,'tiYAxisString',
     +     'U component of wind (m/s)',ierr)
      call NhlFRLSetString(srlist,'tiYAxisSide','Right',ierr)
      call NhlFRLSetFloat(srlist,'tiXAxisFontHeightF',0.02,ierr)
      call NhlFRLSetFloat(srlist,'tiYAxisFontHeightF',0.02,ierr)
      call NhlFRLSetString(srlist,'tiMainFont','helvetica-bold',ierr)
      call NhlFRLSetString(srlist,'tiXAxisFont','helvetica-bold',ierr)
      call NhlFRLSetString(srlist,'tiYAxisFont','helvetica-bold',ierr)
      call NhlFRLSetString(srlist,'tiYAxisFontColor','cyan',ierr)
      call NhlFRLSetString(srlist,'tmYRMinorOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYLMinorOn','False',ierr)
      call NhlFCreate(xy2,'xy2',NhlFXyPlotClass,xworkid,
     +     srlist,ierr)
C
C Create XyPlot object for curve 3 and assign data to it.
C
C Increase the veiwport so the right scale will be about .15 NDC
C right of the other grids.  Plot only the right vertical axis.
C .5NDC = 360 deg lon, thus .65NDC = 360+108 deg lon.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetFloat(srlist,'vpXF',.20,ierr)
      call NhlFRLSetFloat(srlist,'vpYF',.80,ierr)
      call NhlFRLSetFloat(srlist,'vpWidthF',.65,ierr)
      call NhlFRLSetFloat(srlist,'vpHeightF',.6,ierr)
      call NhlFRLSetInteger(srlist,'xyCoordData',field3,ierr)
      call NhlFRLSetString(srlist,'trYReverse','False',ierr)
      call NhlFRLSetFloat(srlist,'trYMaxF',20.,ierr)
      call NhlFRLSetFloat(srlist,'trYMinF',-20.,ierr)
      call NhlFRLSetFloat(srlist,'trXMaxF',468.,ierr)
      call NhlFRLSetFloat(srlist,'trXMinF',0.,ierr)
      call NhlFRLSetString(srlist,'tmYROn','True',ierr)
      call NhlFRLSetString(srlist,'tmYLOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYUseLeft','False',ierr)
      call NhlFRLSetString(srlist,'tmYRLabelsOn','True',ierr)
      call NhlFRLSetFloat(srlist,'tmYRMajorLengthF',.01,ierr)
      call NhlFRLSetFloat(srlist,'tmYRMajorOutwardLengthF',.01,ierr)
      call NhlFRLSetString(srlist,'tmXBOn','False',ierr)
      call NhlFRLSetString(srlist,'tmXTOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYLOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYROn','True',ierr)
      call NhlFRLSetString(srlist,'tmYRMode','Explicit',ierr)
      call NhlFRLSetFloatArray(srlist,'tmYRValues',yrvals2,5,ierr)
      call NhlFRLSetStringArray(srlist,'tmYRLabels',yrlabs2,5,ierr)
      call NhlFRLSetString(srlist,'tmYRLabelFontColor','green',ierr)
      call NhlFRLSetString(srlist,'tiYAxisString',
     +     'V component of wind (m/s)',ierr)
      call NhlFRLSetString(srlist,'tiYAxisSide','Right',ierr)
      call NhlFRLSetFloat(srlist,'tiXAxisFontHeightF',0.02,ierr)
      call NhlFRLSetFloat(srlist,'tiYAxisFontHeightF',0.02,ierr)
      call NhlFRLSetString(srlist,'tiMainString',
     +     'Three Variables with Individual Scales',ierr)
      call NhlFRLSetFloat(srlist,'tiMainFontHeightF',0.030,ierr)
      call NhlFRLSetString(srlist,'tiMainFont','helvetica-bold',ierr)
      call NhlFRLSetString(srlist,'tiYAxisFont','helvetica-bold',ierr)
      call NhlFRLSetString(srlist,'tiYAxisFontColor','green',ierr)
      call NhlFRLSetString(srlist,'tmYRMinorOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYLMinorOn','False',ierr)
      call NhlFRLSetString(srlist,'tmXBMinorOn','False',ierr)
      call NhlFRLSetString(srlist,'tmXTBorderOn','False',ierr)
      call NhlFRLSetString(srlist,'tmXBBorderOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYLBorderOn','False',ierr)
      call NhlFCreate(xy3,'xy3',NhlFXyPlotClass,xworkid,
     +     srlist,ierr)
C
C Get the Data Spec Id of the first plot so we can then change
C the line color.
C
      call NhlFRLClear(grlist)
      call NhlFRLGetInteger(grlist,'xyCoordDataSpec',spec1,ierr)
      call NhlFGetValues(xy1,grlist,ierr)
      
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'xyMonoLineColor','true',ierr)
      call NhlFRLSetString(srlist,'xyLineColor','red',ierr)
      call NhlFSetValues(spec1,srlist,ierr)
C
C Get the Data Spec id of the second plot so we can then change
C the line color.
C
      call NhlFRLClear(grlist)
      call NhlFRLGetInteger(grlist,'xyCoordDataSpec',spec2,ierr)
      call NhlFGetValues(xy2,grlist,ierr)

      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'xyMonoLineColor','true',ierr)
      call NhlFRLSetString(srlist,'xyLineColor','cyan',ierr)
      call NhlFSetValues(spec2,srlist,ierr)
C
C Get the Data Spec id of the third plot so we can then change
C the line color.
C
      call NhlFRLClear(grlist)
      call NhlFRLGetInteger(grlist,'xyCoordDataSpec',spec3,ierr)
      call NhlFGetValues(xy3,grlist,ierr)

      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'xyMonoLineColor','true',ierr)
      call NhlFRLSetString(srlist,'xyLineColor','green',ierr)
      call NhlFSetValues(spec3,srlist,ierr)
C
C Draw all three plots.
C
      call NhlFDraw(xy1,ierr)
      call NhlFDraw(xy2,ierr)
      call NhlFDraw(xy3,ierr)
      call NhlFFrame(xworkid,ierr)
      call NhlFClose
      stop
 104  write (6,*) 'error in opening file: ',filename
      end
