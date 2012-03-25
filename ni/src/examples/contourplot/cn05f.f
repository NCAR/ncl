C
C      $Id: cn05f.f,v 1.9 2010-03-15 22:49:23 haley Exp $
C
C***********************************************************************
C                                                                      *
C                Copyright (C)  1995                                   *
C        University Corporation for Atmospheric Research               *
C     The use of this Software is governed by a License Agreement      *
C                                                                      *
C***********************************************************************
C
C  File:       cn05f.f
C
C  Author:     Tim Scheitln (converted to Fortran by Mary Haley)
C          National Center for Atmospheric Research
C          PO Box 3000, Boulder, Colorado
C
C  Date:       Tue Sep 26 09:44:01 MDT 1995
C
C   Description: Demonstrates how to create a map plot animation with a 
C                contour overlay and labelbar annotation.
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFScalarFieldClass
      external NhlFContourPlotClass
      external NhlFMapPlotClass
      external NhlFtextItemClass
C
C The PLOT_ALL_DATA flag controls whether or not all 31 days of data is 
C read and plotted.  Setting the flag to 1 will cause this script to 
C plot 31 frames.  Otherwise, the script will read only 3 days worth of 
C data and plot 3 frames.
C
      parameter(PLOT_ALL_DATA=0)
C
C Define the maximum number of colors.
C
      parameter(NCOLORS=64)
C
C Declare variables for the HLU routine calls.
C
      integer appid, workid, field1, con1
      integer mapid, lb1id, lb2id
      integer srlist, i, day
C
C Declare variables for defining color map.
C
      integer length(2)
      real cmap(3,NCOLORS)
      data cmap/.000,.000,.000,.000,.000,.000,.700,.700,.700,
     +          .650,.650,.700,.610,.600,.700,.550,.550,.700,
     +          .560,.500,.700,.450,.450,.700,.420,.400,.700,
     +          .350,.350,.700,.300,.300,.700,.250,.250,.700,
     +          .200,.200,.700,.150,.150,.700,.100,.100,.700,
     +          .050,.050,.700,.000,.000,.700,.000,.050,.700,
     +          .000,.100,.700,.000,.150,.700,.000,.200,.700,
     +          .000,.250,.700,.000,.300,.700,.000,.350,.700,
     +          .000,.400,.700,.000,.450,.600,.000,.500,.500,
     +          .000,.550,.400,.000,.600,.300,.000,.650,.200,
     +          .000,.700,.100,.000,.725,.000,.000,.690,.000,
     +          .030,.685,.000,.060,.680,.000,.100,.575,.000,
     +          .130,.570,.000,.160,.565,.000,.550,.550,.000,
     +          .555,.545,.000,.560,.530,.000,.565,.485,.000,
     +          .570,.420,.000,.675,.375,.000,.680,.330,.000,
     +          .690,.300,.000,.700,.285,.000,.700,.270,.000,
     +          .700,.260,.000,.700,.240,.000,.700,.180,.000,
     +          .700,.130,.000,.700,.120,.000,.700,.100,.000,
     +          .700,.090,.000,.750,.090,.000,.800,.090,.000,
     +          .830,.070,.000,.870,.050,.000,.900,.030,.000,
     +          .950,.010,.000,.990,.000,.000,1.00,.000,.000,
     +          1.00,.000,.000/
C
C Create an array that will contain the indices into the 
C colormap defined later.
C
      integer fillindices(NCOLORS-2)
C
C Declare variables for getting information from netCDF file.
C
      integer ncid, t_id, lon_id, lat_id, time_id
      real T(49,40)
      integer lon(49), lat(40)
      integer start(3), count(3), lonlen, latlen, flen, nframes
      character*256 filename
      character*15 daystr
      character*50 recname

      character*7  wks_type
C
C Open the netCDF file.
C
      call gngpat(filename,'data',ierr)
      flen = 17
      do 10 i=1,256
         if( filename(i:i).eq.char(0) ) then
            filename(i:i+flen)='/cdf/meccatemp.cdf'
            goto 15
         endif
 10   continue
C      
C The second argument to 'ncopn' should be NCNOWRIT, but since we
C can't include 'netcdf.inc', we are using the value '0' instead.
C
 15   ncid = ncopn(filename,0,ierr)
C
C Initialize the HLU library and set up resource template.
C
      call NhlFInitialize
      call NhlFRLCreate(srlist,'setrl')
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(srlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'cn05',NhlFAppClass,0,srlist,ierr)

      wks_type = "ncgm"

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkMetaName','./cn05f.ncgm',ierr)
         call NhlFCreate(workid,'cn05Work',
     +        NhlFNcgmWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPause','True',ierr)
         call NhlFCreate(workid,'cn05Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,srlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPSFileName','./cn05f.ps',ierr)
         call NhlFCreate(workid,'cn05Work',
     +        NhlFPSWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPDFFileName','./cn05f.pdf',
     +        ierr)
         call NhlFCreate(workid,'cn05Work',
     +        NhlFPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFileName','./cn05f',
     +        ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(workid,'cn05Work',
     +        NhlFCairoPSPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFileName','./cn05f',
     +        ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(workid,'cn05Work',
     +        NhlFCairoImageWorkstationClass,0,srlist,ierr)
      endif
C
C Assign the colormap to the workstation.
C
      length(1) = 3
      length(2) = NCOLORS
      call NhlFRLClear(srlist)
      call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,ierr)
      call NhlFSetValues(workid,srlist,ierr)
C
C Get the temperature and lat/lon dimensions.
C
      lat_id = ncdid(ncid,'lat',ierr)
      lon_id = ncdid(ncid,'lon',ierr)
      t_id = ncvid(ncid,'t',ierr)
      call ncdinq(ncid,lat_id,recname,latlen,ierr)
      call ncdinq(ncid,lon_id,recname,lonlen,ierr)
      lat_id = ncvid(ncid,'lat',ierr)
      lon_id = ncvid(ncid,'lon',ierr)
C
C Get temperature and lat/lon values.
C
      start(1) = 1
      start(2) = 1
      start(3) = 1
      count(1) = lonlen
      count(2) = latlen
      count(3) = 1
      call ncvgt(ncid,t_id,start,count,T,ierr)
      count(1) = latlen
      call ncvgt(ncid,lat_id,start,count,lat,ierr)
      count(1) = lonlen
      call ncvgt(ncid,lon_id,start,count,lon,ierr)
C
C Create a scalar field object that will be used as the
C dataset for the contour object.
C
      count(1) = lonlen
      count(2) = latlen
      call NhlFRLClear(srlist)
      call NhlFRLSetMDFloatArray(srlist,'sfDataArray',T,2,count,ierr)
      call NhlFRLSetInteger(srlist,'sfXCStartV',lon(1),ierr)
      call NhlFRLSetInteger(srlist,'sfXCEndV',lon(lonlen),ierr)
      call NhlFRLSetInteger(srlist,'sfYCStartV',lat(1),ierr)
      call NhlFRLSetInteger(srlist,'sfYCEndV',lat(latlen),ierr)
      call NhlFCreate(field1,'field1',NhlFscalarFieldClass,appid,srlist,
     +     ierr)
C
C Assign the indices, skipping the first two colormap entries
C that contain the default background and foreground color. 
C
      do 20 i = 1,NCOLORS-2
         fillindices(i) = i+1
 20   continue
C
C Create a ContourPlot object.
C
      call NhlFRLClear(srlist)
C
C Assign the data that was read earlier.
C
      call NhlFRLSetInteger(srlist,'cnScalarFieldData',field1,ierr)
C
C Assign the colormap fill indices
C
      call NhlFRLSetIntegerArray(srlist,'cnFillColors',fillindices,
     +     NCOLORS-2,ierr)
C
C Set the range and spacing of the contour levels.
C
      call NhlFRLSetString(srlist,'cnLevelSelectionMode','ManualLevels',
     +     ierr)
      call NhlFRLSetFloat(srlist,'cnMinLevelValF',195.0,ierr)
      call NhlFRLSetFloat(srlist,'cnMaxLevelValF',328.0,ierr)
      call NhlFRLSetFloat(srlist,'cnLevelSpacingF',2.25,ierr)
C
C Turn on contour fills.
C
      call NhlFRLSetString(srlist,'cnFillOn','True',ierr)
C
C Turn off the contour lines and labels.
C
      call NhlFRLSetString(srlist,'cnLinesOn','False',ierr)
      call NhlFRLSetString(srlist,'cnLineLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'cnHighLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'cnLowLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'cnInfoLabelOn','False',ierr)
C
C Turn on the overlay labelbar.
C
      call NhlFRLSetString(srlist,'pmLabelBarDisplayMode','ALWAYS',ierr)
C
C Set the labelbar size
C
      call NhlFRLSetFloat(srlist,'pmLabelBarHeightF',0.15,ierr)
      call NhlFRLSetFloat(srlist,'pmLabelBarWidthF',0.6,ierr)
C
C Set the location and orientation of the labelbar.
C
      call NhlFRLSetString(srlist,'pmLabelBarSide','bottom',ierr)
      call NhlFRLSetString(srlist,'lbOrientation','horizontal',ierr)
C
C Set the lablebar title, font, and color.
C
      call NhlFRLSetString(srlist,'lbTitleString','Day 1',ierr)
      call NhlFRLSetInteger(srlist,'lbTitleFont',22,ierr)
      call NhlFRLSetString(srlist,'lbTitleFontColor','PaleGreen4',ierr)
C
C Turn off the labelbar perimeter box 
C
      call NhlFRLSetString(srlist,'lbPerimOn','False',ierr)
C
C Turn off lines that separate each color in the labelbar.
C
      call NhlFRLSetInteger(srlist,'lbBoxLinesOn',0,ierr)
C
C Turn off labelbar labels
C
      call NhlFRLSetString(srlist,'lbLabelsOn','False',ierr)
      call NhlFCreate(con1,'con1',NhlFcontourPlotClass,workid,srlist,
     +     ierr)
C
C Create map object.
C
      call NhlFRLClear(srlist)
C
C Allow the map to be stretched along 
C the horizontal and vertical view axes.
C
      call NhlFRLSetString(srlist,'mpShapeMode','FreeAspect',ierr)
C
C Set the viewport position and size. This will
C stretch the map along its axes.
C
      call NhlFRLSetFloat(srlist,'vpXF',0.03,ierr)
      call NhlFRLSetFloat(srlist,'vpYF',0.90,ierr)
      call NhlFRLSetFloat(srlist,'vpWidthF',0.94,ierr)
      call NhlFRLSetFloat(srlist,'vpHeightF',0.68,ierr)
C
C Set the center of projection.
C
      call NhlFRLSetFloat(srlist,'mpCenterLatF',0.0,ierr)
      call NhlFRLSetFloat(srlist,'mpCenterLonF',150.0,ierr)
C
C Set the projection type.
C
      call NhlFRLSetString(srlist,'mpProjection',
     +     'CYLINDRICALEQUIDISTANT',ierr)
C
C Turn off grid and limb lines, labels, and permimeter.
C
      call NhlFRLSetString(srlist,'mpGridAndLimbOn','False',ierr)
      call NhlFRLSetString(srlist,'mpLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'mpPerimOn','False',ierr)
C
C Turn on main title and set its value, font, and color.
C
      call NhlFRLSetString(srlist,'pmTitleDisplayMode','Always',ierr)
      call NhlFRLSetString(srlist,'tiMainString',
     +     'January Global Surface Temperature',ierr)
      call NhlFRLSetInteger(srlist,'tiMainFont',22,ierr)
      call NhlFRLSetString(srlist,'tiMainFontColor','PaleGreen4',ierr)
      call NhlFCreate(mapid,'map',NhlFmapPlotClass,workid,srlist,ierr)
      call NhlFAddOverlay(mapid,con1,-1,ierr)
C
C Create two labels (high and low values) for labelbar.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'txString','195 K',ierr)
      call NhlFRLSetFloat(srlist,'txPosXF',0.05,ierr)
      call NhlFRLSetFloat(srlist,'txPosYF',0.03,ierr)
      call NhlFRLSetInteger(srlist,'txFont',22,ierr)
      call NhlFRLSetString(srlist,'txFontColor','PaleGreen4',ierr)
      call NhlFRLSetFloat(srlist,'txFontHeightF',0.03,ierr)
      call NhlFRLSetString(srlist,'txJust','CENTERLEFT',ierr)
      call NhlFCreate(lb1id,'lbarlo',NhlFtextItemClass,workid,srlist,
     +     ierr)

      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'txString','328 K',ierr)
      call NhlFRLSetFloat(srlist,'txPosXF',0.85,ierr)
      call NhlFRLSetFloat(srlist,'txPosYF',0.03,ierr)
      call NhlFRLSetInteger(srlist,'txFont',22,ierr)
      call NhlFRLSetString(srlist,'txFontColor','PaleGreen4',ierr)
      call NhlFRLSetFloat(srlist,'txFontHeightF',0.03,ierr)
      call NhlFRLSetString(srlist,'txJust','CENTERLEFT',ierr)
      call NhlFCreate(lb2id,'lbarhi',NhlFtextItemClass,workid,srlist,
     +     ierr)
C
C Draw all objects.
C
      write(6,*)'Plotting Day 1'

      call NhlFDraw(mapid,ierr)
      call NhlFDraw(lb1id,ierr)
      call NhlFDraw(lb2id,ierr)
      call NhlFFrame(workid,ierr)
C
C Loop on remaining time steps
C
      if ( PLOT_ALL_DATA.eq.0 ) then
         nframes=2
      else
         time_id = ncdid(ncid,'time',ierr)
         call ncdinq(ncid,time_id,recname,nframes,ierr)
         nframes = nframes -1
      endif
      do 30 i = 1,nframes
         write(6,*)'Plotting Day ',i+1
C
C Read the next data field (next day).
C
        start(1) = 1
        start(2) = 1
        start(3) = i+1
        count(1) = lonlen
        count(2) = latlen
        count(3) = 1
        call ncvgt(ncid,t_id,start,count,T,ierr)
        count(1) = lonlen
        count(2) = latlen
        call NhlFRLClear(srlist)
        call NhlFRLSetMDFloatArray(srlist,'sfDataArray',T,2,count,ierr)
        call NhlFSetValues(field1,srlist,ierr)
C
C Increment day string.
C
        day = i+1
        write(daystr,35)day
 35     format('Day ',i2)
        call NhlFRLClear(srlist)
        call NhlFRLSetString(srlist,'lbTitleString',daystr,ierr)
        call NhlFSetValues(con1,srlist,ierr)
C
C Draw another plot.
C
        call NhlFDraw(mapid,ierr)
        call NhlFDraw(lb1id,ierr)
        call NhlFDraw(lb2id,ierr)
        call NhlFFrame(workid,ierr)

 30   continue
      if(PLOT_ALL_DATA.eq.0 ) then
         write(6,*)'To plot all 31 days in this animation, edit the NCL'
         write(6,*)'script, and set the PLOT_ALL_DATA flag to TRUE.'
      endif
C
C Close the netCDF file.
C
      call ncclos(ncid,ierr)
C
C NhlDestroy destroys the given id and all of its children.
C
      call NhlFRLDestroy(srlist)
      call NhlFDestroy(appid,ierr)
C
C Restores state.
C
      call NhlFClose
      stop
      end
