C
C      $Id: cn16f.f,v 1.5 2010-03-15 22:49:23 haley Exp $
C
C***********************************************************************
C                                                                      *
C                Copyright (C)  1997                                   *
C        University Corporation for Atmospheric Research               *
C     The use of this Software is governed by a License Agreement      *
C                                                                      *
C***********************************************************************
C
C  File:       cn16f.f
C
C   Author:     Bob Lackman
C               National Center for Atmospheric Research
C               PO 3000, Boulder, Colorado
C
C   Date:       Wed Mar 19, 1996
C
C   Description:    Combines a vector fill contour plot, a raster
C                   contour plot, and a map plot on a single frame.
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
      parameter(NLAT=91, NLON=181)
C
C Declare variables for the HLU routine calls.
C
      integer appid, wid, cn, ice, mp, srlist, sst
C
C Declare variables for contour plot fills.
C
      integer fillcolors(17)
      data fillcolors/2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
C
C Declare variables for Tick Mark values.
C
      real xbvalues(13),ylvalues(7)
      character*4 xblabels(13)
      character*3 yllabels(7)
      data xbvalues/30,60,90,120,150,180,210,240,270,300,330,360,390/
      data xblabels/'30E','60E','90E','120E','150E','180','150W',
     +     '120W','90W','60W','30W','0','30E'/
      data ylvalues/-90, -60,-30,0,30,60,90/
      data yllabels/'90S','60S','30S','0','30N','60N','90N'/
C
C Define data variables.
C
      real sstjan(NLON,NLAT), sstshft(NLON,NLAT)
C
C Declare variables for getting information from netCDF file.
C
      integer  ncid, lon_id, lat_id, stid
      real lat(NLAT)
      integer start(3), count(3), lonlen, latlen
      character*256 filename
      character*50 recname
      character*7  wks_type
C
C Default is to create an X11 window.
C
      wks_type = "x11"

C
C Initialize the HLU library and set up resource template.
C A resource file is not used in this example, but if you did
C want one, it would be called "cn16.res".
C
      call NhlFInitialize
      call NhlFRLCreate(srlist,'setrl')
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(srlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'cn16',NhlFAppClass,0,srlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkMetaName','./cn16f.ncgm',ierr)
         call NhlFCreate(wid,'cn16Work',
     +        NhlFNcgmWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an XWorkstation object.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'cn16Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,srlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPSFileName','./cn16f.ps',ierr)
         call NhlFCreate(wid,'cn16Work',
     +        NhlFPSWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPDFFileName','./cn16f.pdf',ierr)
         call NhlFCreate(wid,'cn16Work',
     +        NhlFPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFileName','./cn16f',ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'cn16Work',
     +        NhlFCairoPSPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFileName','./cn16f',ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'cn16Work',
     +        NhlFCairoImageWorkstationClass,0,srlist,ierr)
      endif
C
C Read NCL created NetCDF file containing U and V wind components.
C
      call gngpat(filename,'data',ierr)
      do 10 i=1,256
         if( filename(i:i).eq.char(0) ) then
            filename(i:i+21)='/cdf/sstdata_netcdf.nc'
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
      lat_id = ncdid(ncid,'latitude',ierr)
      lon_id = ncdid(ncid,'longitude',ierr)
      call ncdinq(ncid,lat_id,recname,latlen,ierr)
      call ncdinq(ncid,lon_id,recname,lonlen,ierr)
C
C Read in lat values from netCDF file.
C
      start(1) = 1
      lat_id = ncvid(ncid,'lat',ierr)
      count(1) = latlen
      call ncvgt(ncid,lat_id,start,count,lat,ierr)
C
C Read in sstdata netCDF file.
C
      stid = ncvid(ncid,'sst',ierr)
      start(1) = 1
      start(2) = 1
      start(3) = 1
      count(1) = lonlen
      count(2) = latlen
      count(3) = 1
      call ncvgt(ncid,stid,start,count,sstjan,ierr)
C
C Close the netCDF file.
C
      call ncclos(ncid,ierr)
C
C The input sea surface temperature array of 0 to 360 longitude is
C shifted to 30E to 390 (30E) by array index manipulation.
C
      do j=1,NLAT
         do i=15,180
            sstshft(i-14,j) = sstjan(i+1,j)
         end do
         do i=165,180
            sstshft(i+1,j) = sstjan(i-164,j)
         end do
      end do
C
C Create an sst ScalarField data object.
C
      count(1) = NLON
      count(2) = NLAT
      call NhlFRLClear(srlist)
      call NhlFRLSetMDFloatArray(srlist,'sfDataArray',sstshft,2,count,
     +     ierr)
      call NhlFRLSetFloat(srlist,'sfXCStartV',30.,ierr)
      call NhlFRLSetFloat(srlist,'sfXCEndV',390.,ierr)
      call NhlFRLSetFloat(srlist,'sfYCStartV',lat(1),ierr)
      call NhlFRLSetFloat(srlist,'sfYCEndV',lat(latlen),ierr)
      call NhlFCreate(sst,'sf',NhlFscalarFieldClass,appid,srlist,
     +     ierr)
C
C Create a ContourPlot object.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetInteger(srlist,'cnScalarFieldData',sst,ierr)
      call NhlFRLSetFloat(srlist,'vpXF',.10,ierr)
      call NhlFRLSetFloat(srlist,'vpYF',.80,ierr)
      call NhlFRLSetFloat(srlist,'vpWidthF',.80,ierr)
      call NhlFRLSetFloat(srlist,'vpHeightF',.40,ierr)
      call NhlFRLSetString(srlist,'cnInfoLabelOn','False',ierr)
      call NhlFRLSetString(srlist,'cnHighLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'cnLowLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'cnMonoLineColor','False',ierr)
      call NhlFRLSetString(srlist,'cnLineDrawOrder','predraw',ierr)
      call NhlFRLSetString(srlist,'cnFillDrawOrder','predraw',ierr)
      call NhlFRLSetString(srlist,'cnLabelDrawOrder','predraw',ierr)
      call NhlFRLSetInteger(srlist,'cnLineLabelInterval',2,ierr)
      call NhlFRLSetString(srlist,'cnLineLabelPlacementMode','computed',
     +     ierr)
      call NhlFRLSetString(srlist,'tiMainOn','True',ierr)
      call NhlFRLSetString(srlist,'tiMainString',
     +     'STR:: JANUARY SST CLIMATOLOGY',ierr)
      call NhlFRLSetFloat(srlist,'tiMainFontHeightF',.020,ierr)
      call NhlFRLSetInteger(srlist,'tiMainFont',25,ierr)
      call NhlFRLSetString(srlist,'tmXBMode','EXPLICIT',ierr)
      call NhlFRLSetString(srlist,'tmYLMode','EXPLICIT',ierr)
      call NhlFRLSetFloatArray(srlist,'tmXBValues',xbvalues,13,ierr)
      call NhlFRLSetStringArray(srlist,'tmXBLabels',xblabels,13,ierr)
      call NhlFRLSetFloatArray(srlist,'tmYLValues',ylvalues,7,ierr)
      call NhlFRLSetStringArray(srlist,'tmYLLabels',yllabels,7,ierr)
      call NhlFRLSetString(srlist,'tmXTLabelsOn','True',ierr)
      call NhlFRLSetString(srlist,'tmYRLabelsOn','True',ierr)
      call NhlFRLSetFloat(srlist,'tmXBLabelFontHeightF',.015,ierr)
      call NhlFRLSetFloat(srlist,'tmYLLabelFontHeightF',.015,ierr)
      call NhlFRLSetFloat(srlist,'tmXBMajorOutwardLengthF',.006,ierr)
      call NhlFRLSetFloat(srlist,'tmXBMajorLengthF',.006,ierr)
      call NhlFRLSetFloat(srlist,'tmYLMajorOutwardLengthF',.006,ierr)
      call NhlFRLSetFloat(srlist,'tmYLMajorLengthF',.006,ierr)
      call NhlFRLSetString(srlist,'tmXBMinorOn','False',ierr)
      call NhlFRLSetString(srlist,'tmXTMinorOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYLMinorOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYRMinorOn','False',ierr)
      call NhlFCreate(cn,'cn',NhlFcontourPlotClass,wid,srlist,ierr)
C
C The ice field is added as a raster contour.  Areas without ice
C are colored transparent.
C
      cellsize = .8/360.
      call NhlFRLClear(srlist)
      call NhlFRLSetInteger(srlist,'cnScalarFieldData',sst,ierr)
      call NhlFRLSetFloat(srlist,'vpXF',.10,ierr)
      call NhlFRLSetFloat(srlist,'vpYF',.80,ierr)
      call NhlFRLSetFloat(srlist,'vpWidthF',.80,ierr)
      call NhlFRLSetFloat(srlist,'vpHeightF',.40,ierr)
      call NhlFRLSetString(srlist,'tmXBOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYLOn','False',ierr)
      call NhlFRLSetString(srlist,'tmXBMinorOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYLMinorOn','False',ierr)
      call NhlFRLSetString(srlist,'tmXBLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYLLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'tiMainOn','False',ierr)
      call NhlFRLSetFloat(srlist,'tmXBMajorLengthF',0.,ierr)
      call NhlFRLSetFloat(srlist,'tmYLMajorLengthF',0.,ierr)
      call NhlFRLSetString(srlist,'cnFillOn','True',ierr)
      call NhlFRLSetString(srlist,'cnFillMode','RasterFill',ierr)
      call NhlFRLSetFloat(srlist,'cnRasterCellSizeF',cellsize,ierr)
      call NhlFRLSetFloat(srlist,'cnMinLevelValF',-2.0,ierr)
      call NhlFRLSetIntegerArray(srlist,'cnFillColors',fillcolors,17,
     +     ierr)
      call NhlFRLSetString(srlist,'cnLineLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'cnLinesOn','False',ierr)
      call NhlFRLSetString(srlist,'cnMonoFillColor','False',ierr)
      call NhlFRLSetString(srlist,'cnInfoLabelOn','False',ierr)
      call NhlFRLSetString(srlist,'cnHighLabelsOn','False',ierr)
      call NhlFCreate(ice,'ice',nhlfcontourplotclass,wid,srlist,ierr)
C
C Create a MapPlot object.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetFloat(srlist,'vpXF',.10,ierr)
      call NhlFRLSetFloat(srlist,'vpYF',.80,ierr)
      call NhlFRLSetFloat(srlist,'vpWidthF',.80,ierr)
      call NhlFRLSetFloat(srlist,'vpHeightF',.40,ierr)
      call NhlFRLSetString(srlist,'mpFillOn','True',ierr)
      call NhlFRLSetString(srlist,'mpLabelsOn','False',ierr)
      call NhlFRLSetInteger(srlist,'mpDefaultFillColor',11,ierr)
      call NhlFRLSetInteger(srlist,'mpLandFillColor',11,ierr)
      call NhlFRLSetString(srlist,'mpOutlineOn','False',ierr)
      call NhlFRLSetString(srlist,'mpAreaMaskingOn','True',ierr)
      call NhlFRLSetString(srlist,'mpMaskAreaSpecifiers','Oceans',ierr)
      call NhlFRLSetString(srlist,'mpGridAndLimbOn','False',ierr)
      call NhlFRLSetString(srlist,'mpLimitMode','latlon',ierr)
      call NhlFRLSetFloat(srlist,'mpMinLonF',30.,ierr)
      call NhlFRLSetFloat(srlist,'mpMaxLonF',390.,ierr)
      call NhlFRLSetFloat(srlist,'mpCenterLonF',210.,ierr)
      call NhlFCreate(mp,'mp',nhlfmapplotclass,wid,srlist,ierr)
C
C Draw everything and clean up.
C
      call NhlFDraw(ice,ierr)
      call NhlFDraw(cn,ierr)
      call NhlFDraw(mp,ierr)
      call NhlFFrame(wid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFClose
      stop
      end

