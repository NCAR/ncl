C
C      $Id: cn10f.f,v 1.8 2010-03-15 22:49:23 haley Exp $
C
C***********************************************************************
C                                                                      *
C                Copyright (C)  1995                                   *
C        University Corporation for Atmospheric Research               *
C     The use of this Software is governed by a License Agreement      *
C                                                                      *
C***********************************************************************
C
C   File:       cn10f.f
C 
C   Author:     Fred Clare (converted to Fortran by Mary Haley)
C           National Center for Atmospheric Research
C           PO 3000, Boulder, Colorado
C 
C   Date:       Mon Sep 25 16:07:14 MDT 1995
C 
C   Description:    Reads a netCDF file and produces five plots:
C 
C                      1.)  A quick-and-dirty contour plot.
C                      2.)  An improved contour plot.
C                      3.)  A contour plot of a sub-area of plot 2.)
C                      4.)  An XyPlot that is a slice through the
C                           the contoured area in 2.)
C                      5.)  An overlay of the contour plot in 2.) on
C                           an MapPlot object.
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFScalarFieldClass
      external NhlFCoordArraysClass
      external NhlFContourPlotClass
      external NhlFMapPlotClass
      external NhlFXyPlotClass
C
C Define the maximum number of colors.
C
      parameter(NCOLORS=27)
C
C Declare variables for the HLU routine calls.
C
      integer appid, work_id, field1, field2, con1, con2
      integer dataspec, mapid, y_dataid, xy_id
      integer srlist, grlist, i
      real ymin, ymax, special_value
      data special_value/-9999./
C
C Declare variables for defining color map.
C
      integer length(2), colors(13)
      real cmap(3,NCOLORS)
C
C Declare variables for getting information from netCDF file.
C
      integer ncid, mnd_id, xdim_id, ydim_id
      real mound(18,15), mound2(5,5), xdim(20), ydim(20)
      integer start(2), count(2), xlen, ylen, flen
      character*256 filename
      character*50 recname

      character*7  wks_type
C
C Define the workstation type
C
      wks_type = "x11"
C
C
C Initialize the HLU library and set up resource template.
C
      call NhlFInitialize
      call NhlFRLCreate(srlist,'setrl')
C
C Modify the color map.  Color indices '1' and '2' are the background
C and foreground colors respectively.
C
      cmap(1, 1) = 1.00
      cmap(2, 1) = 1.00
      cmap(3, 1) = 1.00
      cmap(1, 2) = 0.00
      cmap(2, 2) = 0.00
      cmap(3, 2) = 0.00
      cmap(1, 3) = 0.00
      cmap(2, 3) = 0.15
      cmap(3, 3) = 1.00
      cmap(1, 4) = 0.00
      cmap(2, 4) = 0.05
      cmap(3, 4) = 1.00
      cmap(1, 5) = 0.04
      cmap(2, 5) = 0.00
      cmap(3, 5) = 1.00
      cmap(1, 6) = 0.14
      cmap(2, 6) = 0.00
      cmap(3, 6) = 1.00
      cmap(1, 7) = 0.24
      cmap(2, 7) = 0.00
      cmap(3, 7) = 1.00
      cmap(1, 8) = 0.34
      cmap(2, 8) = 0.00
      cmap(3, 8) = 1.00
      cmap(1, 9) = 0.43
      cmap(2, 9) = 0.00
      cmap(3, 9) = 1.00
      cmap(1,10) = 0.53
      cmap(2,10) = 0.00
      cmap(3,10) = 1.00
      cmap(1,11) = 0.63
      cmap(2,11) = 0.00
      cmap(3,11) = 1.00
      cmap(1,12) = 0.73
      cmap(2,12) = 0.00
      cmap(3,12) = 1.00
      cmap(1,13) = 0.83
      cmap(2,13) = 0.00
      cmap(3,13) = 1.00
      cmap(1,14) = 0.92
      cmap(2,14) = 0.00
      cmap(3,14) = 1.00
      cmap(1,15) = 1.00
      cmap(2,15) = 0.00
      cmap(3,15) = 0.98
      cmap(1,16) = 1.00
      cmap(2,16) = 0.00
      cmap(3,16) = 0.88
      cmap(1,17) = 1.00
      cmap(2,17) = 0.00
      cmap(3,17) = 0.78
      cmap(1,18) = 1.00
      cmap(2,18) = 0.00
      cmap(3,18) = 0.68
      cmap(1,19) = 1.00
      cmap(2,19) = 0.00
      cmap(3,19) = 0.59
      cmap(1,20) = 1.00
      cmap(2,20) = 0.00
      cmap(3,20) = 0.49
      cmap(1,21) = 1.00
      cmap(2,21) = 0.00
      cmap(3,21) = 0.39
      cmap(1,22) = 1.00
      cmap(2,22) = 0.00
      cmap(3,22) = 0.29
      cmap(1,23) = 1.00
      cmap(2,23) = 0.00
      cmap(3,23) = 0.20
      cmap(1,24) = 1.00
      cmap(2,24) = 0.00
      cmap(3,24) = 0.10
      cmap(1,25) = 1.00
      cmap(2,25) = 0.00
      cmap(3,25) = 0.00
C
C  Colors used for labels.
C
      cmap(1,26) = 0.00
      cmap(2,26) = 0.00
      cmap(3,26) = 0.00
      cmap(1,27) = 0.40
      cmap(2,27) = 0.00
      cmap(3,27) = 0.40
      length(1) = 3
      length(2) = NCOLORS
C
C Create Application object.  The Application object name is used to
C determine the name of the resource file, which is "cn10.res" in
C this case.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(srlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'cn10',NhlFAppClass,0,srlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkMetaName','./cn10f.ncgm',ierr)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(work_id,'cn10Work',
     +        NhlFNcgmWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPause','True',ierr)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(work_id,'cn10Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,srlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPSFileName','./cn10f.ps',ierr)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(work_id,'cn10Work',
     +        NhlFPSWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPDFFileName','./cn10f.pdf',ierr)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(work_id,'cn10Work',
     +        NhlFPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFileName','./cn10f',ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(work_id,'cn10Work',
     +        NhlFCairoPSPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFileName','./cn10f',ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +        ierr)
         call NhlFCreate(work_id,'cn10Work',
     +        NhlFCairoImageWorkstationClass,0,srlist,ierr)
      endif
C
C Open the netCDF file.
C
      call gngpat(filename,'data',ierr)
      flen = 13
      do 10 i=1,256
         if( filename(i:i).eq.char(0) ) then
            filename(i:i+flen)='/cdf/cn10n.cdf'
            goto 15
         endif
 10   continue
C      
C The second argument to 'ncopn' should be NCNOWRIT, but since we
C can't include 'netcdf.inc', we are using the value '0' instead.
C
 15   ncid = ncopn(filename,0,ierr)
C
C Get the mound length.
C
      xdim_id = ncdid(ncid,'xdim',ierr)
      ydim_id = ncdid(ncid,'ydim',ierr)
      mnd_id = ncvid(ncid,'mound',ierr)
      call ncdinq(ncid,xdim_id,recname,xlen,ierr)
      call ncdinq(ncid,ydim_id,recname,ylen,ierr)
      xdim_id = ncvid(ncid,'xdim',ierr)
      ydim_id = ncvid(ncid,'ydim',ierr)
C
C Get data and dimentions values for the mound.
C
      start(1) = 1
      start(2) = 1
      count(1) = ylen
      count(2) = xlen
      call ncvgt(ncid,mnd_id,start,count,mound,ierr)
      count(1) = xlen
      call ncvgt(ncid,xdim_id,start,count,xdim,ierr)
      count(1) = ylen
      call ncvgt(ncid,ydim_id,start,count,ydim,ierr)
C
C Create a data field.
C
      count(1) = ylen
      count(2) = xlen
      call NhlFRLClear(srlist)
      call NhlFRLSetMDFloatArray(srlist,'sfDataArray',mound,2,count,
     +     ierr)
      call NhlFRLSetFloat(srlist,'sfMissingValueV',special_value,ierr)
      call NhlFRLSetFloat(srlist,'sfXCStartV',ydim(1),ierr)
      call NhlFRLSetFloat(srlist,'sfXCEndV',ydim(ylen),ierr)
      call NhlFRLSetFloat(srlist,'sfYCStartV',xdim(1),ierr)
      call NhlFRLSetFloat(srlist,'sfYCEndV',xdim(xlen),ierr)
      call NhlFCreate(field1,'field1',nhlfscalarfieldclass,appid,srlist,
     +     ierr)
C
C Create a ContourPlot object using the above data field.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetInteger(srlist,'cnScalarFieldData',field1,ierr)
      call NhlFRLSetFloat(srlist,'vpXF',0.15,ierr)
      call NhlFRLSetFloat(srlist,'vpYF',0.9,ierr)
      call NhlFRLSetFloat(srlist,'vpWidthF',0.79,ierr)
      call NhlFRLSetFloat(srlist,'vpHeightF',0.79,ierr)
      call NhlFCreate(con1,'con1',nhlfcontourplotclass,work_id,srlist,
     +     ierr)
C
C Picture 1:  A quick-and-dirty contour plot.
C
      call NhlFDraw(con1,ierr)
      call NhlFFrame(work_id,ierr)
C
C Picture 2:  An improved contour plot.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'cnFillOn','True',ierr)
      call NhlFRLSetString(srlist,'cnMonoFillColor','False',ierr)
      call NhlFRLSetFloat(srlist,'cnLevelSpacingF',2.0,ierr)
      call NhlFRLSetFloat(srlist,'cnLevelSpacingF',2.0,ierr)
      call NhlFRLSetString(srlist,'cnSmoothingOn','True',ierr)
      call NhlFRLSetFloat(srlist,'cnMaxPointDistanceF',0.0,ierr)
      call NhlFRLSetInteger(srlist,'cnLineColor',0,ierr)
      call NhlFRLSetInteger(srlist,'cnLineLabelFontColor',25,ierr)
      call NhlFRLSetInteger(srlist,'cnLineLabelFont',22,ierr)
      call NhlFRLSetFloat(srlist,'cnLineLabelFontHeightF',.02,ierr)
      call NhlFRLSetInteger(srlist,'cnLineLabelInterval',3,ierr)
      call NhlFRLSetString(srlist,'cnLineLabelPlacementMode','COMPUTED',
     +     ierr)
      call NhlFRLSetInteger(srlist,'cnHighLabelFont',22,ierr)
      call NhlFRLSetFloat(srlist,'cnHighLabelFontHeightF',.025,ierr)
      call NhlFRLSetInteger(srlist,'cnHighLabelFontColor',25,ierr)
      call NhlFRLSetString(srlist,'cnInfoLabelOn','False',ierr)

      call NhlFRLSetInteger(srlist,'tmXBMinorPerMajor',2,ierr)

      call NhlFRLSetInteger(srlist,'tmYLMajorLineColor',26,ierr)
      call NhlFRLSetInteger(srlist,'tmYLMinorLineColor',26,ierr)
      call NhlFRLSetFloat(srlist,'tmYLMinorThicknessF',2.,ierr)
      call NhlFRLSetInteger(srlist,'tmXBMajorLineColor',26,ierr)
      call NhlFRLSetInteger(srlist,'tmXBMinorLineColor',26,ierr)
      call NhlFRLSetFloat(srlist,'tmXBMinorThicknessF',2.,ierr)

      call NhlFRLSetInteger(srlist,'tmXBLabelFont',21,ierr)
      call NhlFRLSetFloat(srlist,'tmXBLabelFontHeightF',.03,ierr)
      call NhlFRLSetInteger(srlist,'tmXBLabelFontColor',26,ierr)

      call NhlFRLSetInteger(srlist,'tmYLLabelFont',21,ierr)
      call NhlFRLSetFloat(srlist,'tmYLLabelFontHeightF',.03,ierr)
      call NhlFRLSetInteger(srlist,'tmYLLabelFontColor',26,ierr)

      call NhlFRLSetInteger(srlist,'tmBorderLineColor',26,ierr)

      call NhlFRLSetString(srlist,'tiMainOn','True',ierr)
      call NhlFRLSetInteger(srlist,'tiMainFont',26,ierr)
      call NhlFRLSetFloat(srlist,'tiMainFontHeightF',.04,ierr)
      call NhlFRLSetString(srlist,'tiMainString','The Hot Zone',ierr)
      call NhlFRLSetFloat(srlist,'tiMainOffsetYF',-0.025,ierr)
      call NhlFRLSetInteger(srlist,'tiMainFontColor',26,ierr)
      call NhlFSetValues(con1,srlist,ierr)
      call NhlFDraw(con1,ierr)
      call NhlFFrame(work_id,ierr)
C
C Picture 3:  Zero in on the top of the mound.
C
      start(1) = 5
      start(2) = 3
      count(1) = 5
      count(2) = 5
      call ncvgt(ncid,mnd_id,start,count,mound2,ierr)

      call NhlFRLClear(srlist)
      call NhlFRLSetMDFloatArray(srlist,'sfDataArray',mound2,2,count,
     +     ierr)
      call NhlFRLSetFloat(srlist,'sfXCStartV',-20.,ierr)
      call NhlFRLSetFloat(srlist,'sfXCEndV',20.,ierr)
      call NhlFRLSetFloat(srlist,'sfYCStartV', -20.,ierr)
      call NhlFRLSetFloat(srlist,'sfYCEndV',20.,ierr)
      call NhlFSetValues(field1,srlist,ierr)

      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'tiMainString','A closer look',ierr)
      call NhlFRLSetInteger(srlist,'tmXBMinorPerMajor',4,ierr)
      call NhlFRLSetInteger(srlist,'tmYLMinorPerMajor',4,ierr)
      call NhlFRLSetFloat(srlist,'cnLevelSpacingF',0.15,ierr)
      do 20 i = 1,11
         colors(i) = i+8
 20   continue
      colors(12) = 21
      colors(13) = 23
      call NhlFRLSetIntegerArray(srlist,'cnFillColors',colors,13,ierr)
      call NhlFSetValues(con1,srlist,ierr)
      call NhlFDraw(con1,ierr)
      call NhlFFrame(work_id,ierr)
C
C Picture 4:  Plot an XyPlot of X values for a specific Y value.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetFloatArray(srlist,'caXArray',ydim,ylen,ierr)
      call NhlFRLSetFloatArray(srlist,'caYArray',mound(1,6),ylen,ierr)
      call NhlFCreate(y_dataid,'xyData',nhlfcoordarraysclass,appid,
     +     srlist,ierr)

      call NhlFRLClear(srlist)
      call NhlFRLSetFloat(srlist,'vpXF',.2,ierr)
      call NhlFRLSetFloat(srlist,'vpYF',.85,ierr)
      call NhlFRLSetFloat(srlist,'vpWidthF',.7,ierr)
      call NhlFRLSetFloat(srlist,'vpHeightF',.7,ierr)

      call NhlFRLSetString(srlist,'xyComputeYMax','False',ierr)
      call NhlFRLSetString(srlist,'xyComputeYMin','False',ierr)

      call NhlFRLSetString(srlist,'tiMainOn','True',ierr)
      call NhlFRLSetInteger(srlist,'tiMainFont',26,ierr)
      call NhlFRLSetFloat(srlist,'tiMainFontHeightF',.0275,ierr)
      call NhlFRLSetString(srlist,'tiMainString','For X = 10.',ierr)
      call NhlFRLSetFloat(srlist,'tiMainOffsetYF',-0.00,ierr)
      call NhlFRLSetInteger(srlist,'tiMainFontColor',26,ierr)
      call NhlFRLSetString(srlist,'tiXAxisString','Y',ierr)
      call NhlFRLSetInteger(srlist,'tiXAxisFont',26,ierr)
      call NhlFRLSetInteger(srlist,'tiXAxisFontColor',26,ierr)
      call NhlFRLSetFloat(srlist,'tiXAxisFontHeightF',.025,ierr)
      call NhlFRLSetString(srlist,'tiYAxisString','F(Y)',ierr)
      call NhlFRLSetInteger(srlist,'tiYAxisFont',26,ierr)
      call NhlFRLSetInteger(srlist,'tiYAxisFontColor',26,ierr)
      call NhlFRLSetFloat(srlist,'tiYAxisFontHeightF',.025,ierr)

      call NhlFRLSetString(srlist,'tmXTBorderOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYRBorderOn','False',ierr)
      call NhlFRLSetString(srlist,'tmXTOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYROn','False',ierr)
      call NhlFRLSetInteger(srlist,'tmYLMajorLineColor',26,ierr)
      call NhlFRLSetInteger(srlist,'tmYLMinorLineColor',26,ierr)
      call NhlFRLSetFloat(srlist,'tmYLMinorThicknessF',2.,ierr)
      call NhlFRLSetInteger(srlist,'tmXBMajorLineColor',26,ierr)
      call NhlFRLSetInteger(srlist,'tmXBMinorLineColor',26,ierr)
      call NhlFRLSetFloat(srlist,'tmXBMinorThicknessF',2.,ierr)

      call NhlFRLSetInteger(srlist,'tmXBMinorPerMajor',2,ierr)
      call NhlFRLSetInteger(srlist,'tmYLMinorPerMajor',2,ierr)
      call NhlFRLSetInteger(srlist,'tmBorderLineColor',26,ierr)

      call NhlFRLSetInteger(srlist,'tmXBLabelFont',21,ierr)
      call NhlFRLSetFloat(srlist,'tmXBLabelFontHeightF',.025,ierr)
      call NhlFRLSetInteger(srlist,'tmXBLabelFontColor',26,ierr)
      call NhlFRLSetInteger(srlist,'tmYLLabelFont',21,ierr)
      call NhlFRLSetFloat(srlist,'tmYLLabelFontHeightF',.025,ierr)
      call NhlFRLSetInteger(srlist,'tmYLLabelFontColor',26,ierr)
      call NhlFCreate(xy_id,'XyPlotData',nhlfxyplotclass,work_id,srlist,
     +     ierr)

      call NhlFAddData(xy_id,'xyCoordData',y_dataid,dataspec)
      
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'xyMonoLineColor','True',ierr)
      call NhlFRLSetInteger(srlist,'xyLineColor',24,ierr)
      call NhlFRLSetFloat(srlist,'xyLineThicknessF',2.,ierr)
      call NhlFSetValues(dataspec,srlist,ierr)

      call NhlFRLCreate(grlist,'getrl')
      call NhlFRLClear(grlist)
      call NhlFRLGetFloat(grlist,'tmYLTickStartF',ymin,ierr)
      call NhlFRLGetFloat(grlist,'tmYLTickEndF',ymax,ierr)
      call NhlFGetValues(xy_id,grlist,ierr)

      call NhlFRLClear(srlist)
      call NhlFRLSetFloat(srlist,'trYMinF',ymin,ierr)
      call NhlFRLSetFloat(srlist,'trYMaxF',ymax,ierr)
      call NhlFSetValues(xy_id,srlist,ierr)

      call NhlFDraw(xy_id,ierr)
      call NhlFFrame(work_id,ierr)
C
C Picture 5:  Overlay the mound on top of Colorado.
C
      call NhlFRLClear(srlist)

      call NhlFRLSetString(srlist,'pmTitleDisplayMode','always',ierr)

      call NhlFRLSetString(srlist,'tiMainOn','True',ierr)
      call NhlFRLSetInteger(srlist,'tiMainFont',26,ierr)
      call NhlFRLSetFloat(srlist,'tiMainFontHeightF',0.037,ierr)
      call NhlFRLSetFloat(srlist,'tiMainOffsetYF',-0.00,ierr)
      call NhlFRLSetInteger(srlist,'tiMainFontColor',26,ierr)
      call NhlFRLSetString(srlist,'tiMainString','Mound over Colorado',
     +     ierr)

      call NhlFRLSetFloat(srlist,'vpXF',.1,ierr)
      call NhlFRLSetFloat(srlist,'vpYF',.9,ierr)
      call NhlFRLSetFloat(srlist,'vpWidthF',.79,ierr)
      call NhlFRLSetFloat(srlist,'vpHeightF',.79,ierr)

      call NhlFRLSetString(srlist,'mpFillOn','False',ierr)
      call NhlFRLSetString(srlist,'mpOutlineBoundarySets',
     +     'allBoundaries',ierr)
      call NhlFRLSetString(srlist,'mpProjection','LambertConformal',
     +     ierr)
      call NhlFRLSetFloat(srlist,'mpLambertParallel1F',30.,ierr)
      call NhlFRLSetFloat(srlist,'mpLambertParallel2F',45.,ierr)
      call NhlFRLSetFloat(srlist,'mpLambertMeridianF',-100.,ierr)
      call NhlFRLSetString(srlist,'mpLimitMode','LatLon',ierr)
      call NhlFRLSetFloat(srlist,'mpMinLatF',33.,ierr)
      call NhlFRLSetFloat(srlist,'mpMaxLatF',44.,ierr)
      call NhlFRLSetFloat(srlist,'mpMinLonF',-115.,ierr)
      call NhlFRLSetFloat(srlist,'mpMaxLonF',-93.,ierr)
      call NhlFRLSetInteger(srlist,'mpUSStateLineColor',25,ierr)
      call NhlFRLSetFloat(srlist,'mpUSStateLineThicknessF',2.,ierr)
      call NhlFRLSetString(srlist,'mpPerimOn','True',ierr)
      call NhlFRLSetString(srlist,'mpGridAndLimbOn','False',ierr)
      call NhlFRLSetInteger(srlist,'mpPerimLineColor',26,ierr)
      call NhlFRLSetFloat(srlist,'mpPerimLineThicknessF',2.,ierr)
      call NhlFCreate(mapid,'Map0',nhlfmapplotclass,work_id,srlist,ierr)

      call NhlFRLClear(srlist)
      count(1) = ylen
      count(2) = xlen
      call NhlFRLSetMDFloatArray(srlist,'sfDataArray',mound,2,count,
     +     ierr)
      call NhlFRLSetFloat(srlist,'sfXCStartV',-109.05,ierr)
      call NhlFRLSetFloat(srlist,'sfXCEndV',-102.05,ierr)
      call NhlFRLSetFloat(srlist,'sfYCStartV',37.,ierr)
      call NhlFRLSetFloat(srlist,'sfYCEndV',41.,ierr)
      call NhlFCreate(field2,'field2',nhlfscalarfieldclass,appid,srlist,
     +     ierr)

      call NhlFRLClear(srlist)
      call NhlFRLSetInteger(srlist,'cnScalarFieldData',field2,ierr)
      call NhlFRLSetString(srlist,'cnFillOn','True',ierr)
      call NhlFRLSetString(srlist,'cnMonoFillColor','False',ierr)
      call NhlFRLSetFloat(srlist,'cnLevelSpacingF',8.0,ierr)
      call NhlFRLSetString(srlist,'cnSmoothingOn','True',ierr)
      call NhlFRLSetFloat(srlist,'cnMaxPointDistanceF',0.0,ierr)
      call NhlFRLSetInteger(srlist,'cnLineColor',0,ierr)
      call NhlFRLSetString(srlist,'cnLineLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'cnHighLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'cnInfoLabelOn','False',ierr)
      colors(1) = 3
      colors(2) = 7
      colors(3) = 11
      colors(4) = 15
      colors(5) = 19
      colors(6) = 23
      call NhlFRLSetIntegerArray(srlist,'cnFillColors',colors,6,ierr)
      call NhlFCreate(con2,'con2',nhlfcontourplotclass,work_id,srlist,
     +     ierr)
      call NhlFAddOverlay(mapid,con2,-1,ierr)
      call NhlFDraw(mapid,ierr)
      call NhlFFrame(work_id,ierr)
C
C Close the netCDF file.
C
      call ncclos(ncid,ierr)
C
C NhlDestroy destroys the given id and all of its children
C
      call NhlFRLDestroy(srlist)
      call NhlFDestroy(appid,ierr)
C
C Restores state.
C
      call NhlFClose
      stop
      end

