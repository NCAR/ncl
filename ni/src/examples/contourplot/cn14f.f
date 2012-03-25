C
C      $Id: cn14f.f,v 1.5 2010-03-15 22:49:23 haley Exp $
C
C***********************************************************************
C                                                                      *
C                Copyright (C)  1997                                   *
C        University Corporation for Atmospheric Research               *
C     The use of this Software is governed by a License Agreement      *
C                                                                      *
C***********************************************************************
C
C  File:       cn14f.f
C
C   Author:     Bob Lackman
C               National Center for Atmospheric Research
C               PO 3000, Boulder, Colorado
C
C   Date:       Fri Feb 16, 1996
C
C   Description:    Reads two netCDF variables, U and V winds, and produces
C                   a contour plot of the wind speed.
C
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
      parameter(NPTS=73, NCOLORS=16)
C
C Declare variables for the HLU routine calls.
C
      integer appid, workid, cndata, cn_plot, map, srlist
C
C Declare variables for color map
C
      integer length(2)
      real cmap(3,NCOLORS)
C
C Declare variables for contour plot fills.
C
      integer fillcolors(14)
      data fillcolors/15,14,2,3,4,7,6,8,9,10,11,13,12,0/
C
C Declare variables for Tick Mark values.
C
      real xbvalues(7),ylvalues(7)
      character*4 xblabels(7)
      character*3 yllabels(7)
      data xbvalues/-180.,-120.,-60.,0.,60.,120.,180./
      data ylvalues/-90., -60.,-30.,0.,30.,60.,90./
      data xblabels/'180W','120W','60W','0','60E','120E','180E'/
      data yllabels/'90S','60S','30S','EQ','30N','60N','90N'/
C
C Declare variables to hold U, V, and wind speed data.
C
      real u(NPTS,NPTS), v(NPTS,NPTS), spd(NPTS,NPTS)
C
C Declare variables for getting information from netCDF file.
C
      integer  ncid, lon_id, lat_id, uid, vid
      real lon(NPTS), lat(NPTS)
      integer start(3), count(3), lonlen, latlen
      character*256 filename
      character*50 recname
      character*7  wks_type
C
C Define the workstation type
C
      wks_type = "x11"

C
C Generate a color map.
C
      cmap(1,1) = 1.00  ! white
      cmap(2,1) = 1.00
      cmap(3,1) = 1.00
      cmap(1,2) = 0.00  ! black
      cmap(2,2) = 0.00
      cmap(3,2) = 0.00
      cmap(1,3) = 0.00  ! blue
      cmap(2,3) = 0.00
      cmap(3,3) = 1.00
      cmap(1,4) = 0.20  ! sky blue
      cmap(2,4) = 0.56
      cmap(3,4) = 0.80
      cmap(1,5) = 0.00  ! cyan
      cmap(2,5) = 1.00
      cmap(3,5) = 1.00
      cmap(1,6) = 0.50  ! blue magenta
      cmap(2,6) = 0.00
      cmap(3,6) = 1.00
      cmap(1,7) = 0.00  ! green
      cmap(2,7) = 1.00
      cmap(3,7) = 0.00
      cmap(1,8) = 0.14  ! forest green
      cmap(2,8) = 0.56
      cmap(3,8) = 0.14
      cmap(1,9) = 1.00  ! yellow
      cmap(2,9) = 1.00
      cmap(3,9) = 0.00
      cmap(1,10) = 1.00  ! orange
      cmap(2,10) = 0.50
      cmap(3,10) = 0.00
      cmap(1,11) = 1.00 ! magenta
      cmap(2,11) = 0.00
      cmap(3,11) = 1.00
      cmap(1,12) = 1.00 ! red
      cmap(2,12) = 0.00
      cmap(3,12) = 0.00
      cmap(1,13) = 0.65 ! brown
      cmap(2,13) = 0.16
      cmap(3,13) = 0.16
      cmap(1,14) = 0.86 ! tan
      cmap(2,14) = 0.58
      cmap(3,14) = 0.44
      cmap(1,15) = 0.66 ! light gray
      cmap(2,15) = 0.66
      cmap(3,15) = 0.66
      cmap(1,16) = 0.40 ! dark gray
      cmap(2,16) = 0.40
      cmap(3,16) = 0.40
C
C Initialize the HLU library and set up resource template.
C A resource file is not used in this example, but if you did
C want one, it would be called "cn14.res".
C
      call NhlFInitialize
      call NhlFRLCreate(srlist,'setrl')
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(srlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'cn14',NhlFAppClass,0,srlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkMetaName','./cn14f.ncgm',ierr)
         call NhlFCreate(workid,'cn14Work',
     +        NhlFNcgmWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an XWorkstation object.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPause','True',ierr)
         call NhlFCreate(workid,'cn14Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,srlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPSFileName','./cn14f.ps',ierr)
         call NhlFCreate(workid,'cn14Work',
     +        NhlFPSWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPDFFileName','./cn14f.pdf',ierr)
         call NhlFCreate(workid,'cn14Work',
     +        NhlFPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFileName','./cn14f',ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(workid,'cn14Work',
     +        NhlFCairoPSPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFileName','./cn14f',ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(workid,'cn14Work',
     +        NhlFCairoImageWorkstationClass,0,srlist,ierr)
      endif
C
C Set color map resource.
C
      length(1) = 3
      length(2) = NCOLORS
      call NhlFRLClear(srlist)
      call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,length,
     +     ierr)
      call NhlFSetValues(workid,srlist,ierr)
C
C Read NCL created NetCDF file containing U and V wind components.
C
      call gngpat(filename,'data',ierr)
      do 10 i=1,256
         if( filename(i:i).eq.char(0) ) then
            filename(i:i+17)='/cdf/941110_UV.cdf'
            goto 15
         endif
 10   continue
C      
C The second argument to 'ncopn' should be NCNOWRIT, but since we
C can't include 'netcdf.inc', we are using the value '0' instead.
C
 15   ncid = ncopn(filename,0,ierr)
C
C Get the lat/lon dimensions.
C
      lat_id = ncdid(ncid,'lat',ierr)
      lon_id = ncdid(ncid,'lon',ierr)
      call ncdinq(ncid,lat_id,recname,latlen,ierr)
      call ncdinq(ncid,lon_id,recname,lonlen,ierr)
C
C Read in lat/lon values from netCDF file.
C
      start(1) = 1
      lat_id = ncvid(ncid,'lat',ierr)
      count(1) = latlen
      call ncvgt(ncid,lat_id,start,count,lat,ierr)

      lon_id = ncvid(ncid,'lon',ierr)
      count(1) = lonlen
      call ncvgt(ncid,lon_id,start,count,lon,ierr)
C
C Read in u and v from netCDF file.
C
      uid = ncvid(ncid,'u',ierr)
      vid = ncvid(ncid,'v',ierr)
      start(1) = 1
      start(2) = 1
      count(1) = lonlen
      count(2) = latlen
      call ncvgt(ncid,uid,start,count,u,ierr)
      call ncvgt(ncid,vid,start,count,v,ierr)
C
C Close the netCDF file.
C
      call ncclos(ncid,ierr)
C
C Compute the wind speed.
C
      do i=1,lonlen
         do j=1,latlen
            spd(i,j) = sqrt(u(i,j)**2 + v(i,j)**2)
         enddo
      enddo
C
C Create the ScalarField object needed by ContourPlot.
C
      count(1) = NPTS
      count(2) = NPTS
      call NhlFRLClear(srlist)
      call NhlFRLSetMDFloatArray(srlist,'sfDataArray',spd,2,count,ierr)
      call NhlFRLSetFloat(srlist,'sfXCStartV',lon(1),ierr)
      call NhlFRLSetFloat(srlist,'sfXCEndV',lon(lonlen),ierr)
      call NhlFRLSetFloat(srlist,'sfYCStartV',lat(1),ierr)
      call NhlFRLSetFloat(srlist,'sfYCEndV',lat(latlen),ierr)
      call NhlFCreate(cndata,'cndata',NhlFscalarFieldClass,appid,srlist,
     +     ierr)
C
C Create the ContourPlot object and assign data to it.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetFloat(srlist,'vpXF',.10,ierr)
      call NhlFRLSetFloat(srlist,'vpYF',.80,ierr)
      call NhlFRLSetFloat(srlist,'vpWidthF',.80,ierr)
      call NhlFRLSetFloat(srlist,'vpHeightF',.40,ierr)
      call NhlFRLSetInteger(srlist,'cnScalarFieldData',cndata,ierr)
      call NhlFRLSetString(srlist,'cnFillOn','True',ierr)
      call NhlFRLSetIntegerArray(srlist,'cnFillColors',fillcolors,14,
     +     ierr)
C
C Set the range and spacing of the contour levels.
C
      call NhlFRLSetString(srlist,'cnLevelSelectionMode','ManualLevels',
     +     ierr)
      call NhlFRLSetFloat(srlist,'cnMinLevelValF',2.0,ierr)
      call NhlFRLSetFloat(srlist,'cnMaxLevelValF',28.0,ierr)
      call NhlFRLSetFloat(srlist,'cnLevelSpacingF',3.0,ierr)
C
C Turn off the contour lines and labels.
C
      call NhlFRLSetString(srlist,'cnLinesOn','False',ierr)
      call NhlFRLSetString(srlist,'cnLineLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'cnHighLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'cnLowLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'cnInfoLabelOn','False',ierr)
C
C  Set resources of the inherited Title objects.
C
      call NhlFRLSetString(srlist,'tiXAxisString','Longitude',ierr)
      call NhlFRLSetString(srlist,'tiXAxisFont','helvetica',ierr)
      call NhlFRLSetFloat(srlist,'tiXAxisFontHeightF',.02,ierr)
      call NhlFRLSetString(srlist,'tiYAxisString','Latitude',ierr)
      call NhlFRLSetString(srlist,'tiYAxisFont','helvetica',ierr)
      call NhlFRLSetFloat(srlist,'tiYAxisFontHeightF',.02,ierr)
      call NhlFRLSetString(srlist,'tiMainString','Wind Speed',ierr)
      call NhlFRLSetString(srlist,'tiMainFont','helvetica',ierr)
      call NhlFRLSetFloat(srlist,'tiMainFontHeightF',.03,ierr)
C
C Set resources of the inherited TickMark object.
C
      call NhlFRLSetString(srlist,'tmXBMode','EXPLICIT',ierr)
      call NhlFRLSetFloatArray(srlist,'tmXBValues',xbvalues,7,ierr)
      call NhlFRLSetStringArray(srlist,'tmXBLabels',xblabels,7,ierr)
      call NhlFRLSetFloatArray(srlist,'tmYLValues',ylvalues,7,ierr)
      call NhlFRLSetStringArray(srlist,'tmYLLabels',yllabels,7,ierr)
      call NhlFRLSetString(srlist,'tmYLMode','EXPLICIT',ierr)
      call NhlFRLSetFloat(srlist,'tmXBLabelFontHeightF',.016,ierr)
      call NhlFRLSetFloat(srlist,'tmYLLabelFontHeightF',.016,ierr)
      call NhlFRLSetString(srlist,'tmXBLabelFont','times-roman',ierr)
      call NhlFRLSetString(srlist,'tmYLLabelFont','times-roman',ierr)
      call NhlFRLSetString(srlist,'tmXBMinorOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYLMinorOn','False',ierr)
      call NhlFRLSetString(srlist,'pmLabelBarDisplayMode','ALWAYS',ierr)
      call NhlFRLSetFloat(srlist,'pmLabelBarHeightF',.15,ierr)
      call NhlFRLSetFloat(srlist,'pmLabelBarWidthF',.8,ierr)
      call NhlFRLSetString(srlist,'pmLabelBarSide','bottom',ierr)
      call NhlFRLSetString(srlist,'lbOrientation','horizontal',ierr)
      call NhlFRLSetInteger(srlist,'lbBoxLinesOn',0,ierr)
      call NhlFRLSetString(srlist,'lbLabelsOn','True',ierr)
      call NhlFRLSetString(srlist,'lbPerimOn','False',ierr)
      call NhlFRLSetString(srlist,'lbAutoManage','False',ierr)
      call NhlFRLSetFloat(srlist,'lbLabelFontHeightF',0.015,ierr)
      call NhlFRLSetString(srlist,'lbLabelFont','times-roman',ierr)
      call NhlFCreate(cn_plot,'cn_plot',NhlFcontourPlotClass,workid,
     +     srlist,ierr)
C
C Create a MapPlot object.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetFloat(srlist,'vpXF',.10,ierr)
      call NhlFRLSetFloat(srlist,'vpYF',.80,ierr)
      call NhlFRLSetFloat(srlist,'vpWidthF',.80,ierr)
      call NhlFRLSetFloat(srlist,'vpHeightF',.40,ierr)
      call NhlFRLSetString(srlist,'mpFillOn','True',ierr)
      call NhlFRLSetString(srlist,'mpLandFillColor','Black',ierr)
      call NhlFRLSetString(srlist,'mpOceanFillColor','Transparent',ierr)
      call NhlFRLSetString(srlist,'mpInlandWaterFillColor','Black',ierr)
      call NhlFRLSetString(srlist,'mpGridLineColor','LightGray',ierr)
      call NhlFRLSetString(srlist,'mpGeophysicalLineColor','Black',ierr)
      call NhlFRLSetFloat(srlist,'mpGeophysicalLineThicknessF',1.,ierr)
      call NhlFCreate(map,'map',nhlfmapplotclass,workid,srlist,
     +     ierr)
C
C Notice that we are not doing an overlay here! We are simply drawing
C the ContourPlot first, and then the Map Plot.
C
      call NhlFDraw(cn_plot,ierr)
      call NhlFDraw(map,ierr)
      call NhlFFrame(workid,ierr)
      call NhlFClose
      stop
      end

