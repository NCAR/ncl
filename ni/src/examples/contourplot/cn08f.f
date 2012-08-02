C
C      $Id: cn08f.f,v 1.8 2010-03-15 22:49:23 haley Exp $
C
C***********************************************************************
C                                                                      *
C                Copyright (C)  1995                                   *
C        University Corporation for Atmospheric Research               *
C     The use of this Software is governed by a License Agreement      *
C                                                                      *
C***********************************************************************
C
C  File:       cn08f.f
C
C  Author:     Ethan Alpert (converted to Fortran by Mary Haley)
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Thu Sep 28 11:51:33 MDT 1995
C
C  Description:    Draws a vertical profile of temperature for
C                  longitudes separated by 5 degrees.
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
      parameter(NCOLORS=23)
C
C Declare variables for the HLU routine calls.
C
      integer appid, workid, field1, con1
      integer srlist, i, j, k
      real   cmap(3,NCOLORS)
C
C Declare variables for getting information from netCDF file.
C
      integer  ncid, lon_id, lat_id, level_id, temp_id
      real temp(33,10), spv
      real lon(36), lat(33), level(10)
      real min_lat, min_level, max_lat, max_level
      integer start(4), count(4), lonlen, latlen, levellen
      character*256 filename
      character*50 string
      character*50 recname
      character*7  wks_type
C
C Default is to create an NCGM file.
C
      wks_type = "ncgm"

C
C Initialize the HLU library and set up resource template.
C
      call NhlFInitialize
      call NhlFRLCreate(srlist,'setrl')
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(srlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'cn08',NhlFAppClass,0,srlist,ierr)

      cmap(1,1) = 0.0
      cmap(2,1) = 0.0
      cmap(3,1) = 0.0
      cmap(1,2) = 1.0
      cmap(2,2) = 1.0
      cmap(3,2) = 1.0
      cmap(1,3) = 1.0
      cmap(2,3) = 1.0
      cmap(3,3) = 1.0
      cmap(1,4) = 1.0
      cmap(2,4) = 0.0
      cmap(3,4) = 0.0
      cmap(1,5) = 0.0
      cmap(2,5) = 1.0
      cmap(3,5) = 0.0
      cmap(1,6) = 0.0
      cmap(2,6) = 0.0
      cmap(3,6) = 1.0
      cmap(1,7) = 1.0
      cmap(2,7) = 1.0
      cmap(3,7) = 0.0
      cmap(1,8) = 0.0
      cmap(2,8) = 1.0
      cmap(3,8) = 1.0
      cmap(1,9) = 1.0
      cmap(2,9) = 0.0
      cmap(3,9) = 1.0
      cmap(1,10) = 0.5
      cmap(2,10) = 0.0
      cmap(3,10) = 0.0
      cmap(1,11) = 0.5
      cmap(2,11) = 1.0
      cmap(3,11) = 1.0
      cmap(1,12) = 0.0
      cmap(2,12) = 0.0
      cmap(3,12) = 0.5
      cmap(1,13) = 1.0
      cmap(2,13) = 1.0
      cmap(3,13) = 0.5
      cmap(1,14) = 0.5
      cmap(2,14) = 0.0
      cmap(3,14) = 1.0
      cmap(1,15) = 1.0
      cmap(2,15) = 0.5
      cmap(3,15) = 0.0
      cmap(1,16) = 0.0
      cmap(2,16) = 0.5
      cmap(3,16) = 1.0
      cmap(1,17) = 0.5
      cmap(2,17) = 1.0
      cmap(3,17) = 0.0
      cmap(1,18) = 0.5
      cmap(2,18) = 0.0
      cmap(3,18) = 0.5
      cmap(1,19) = 0.5
      cmap(2,19) = 1.0
      cmap(3,19) = 0.5
      cmap(1,20) = 1.0
      cmap(2,20) = 0.5
      cmap(3,20) = 1.0
      cmap(1,21) = 0.0
      cmap(2,21) = 0.5
      cmap(3,21) = 0.0
      cmap(1,22) = 0.5
      cmap(2,22) = 0.5
      cmap(3,22) = 1.0
      cmap(1,23) = 1.0
      cmap(2,23) = 0.0
      cmap(3,23) = 0.5

      count(1) = 3
      count(2) = NCOLORS
      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,count,
     +        ierr)
         call NhlFRLSetString(srlist,'wkMetaName','./cn08f.ncgm',ierr)
         call NhlFCreate(workid,'cn08Work',
     +        NhlFNcgmWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,count,
     +        ierr)
         call NhlFRLSetString(srlist,'wkPause','True',ierr)
         call NhlFCreate(workid,'cn08Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,srlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,count,
     +        ierr)
         call NhlFRLSetString(srlist,'wkPSFileName','./cn08f.ps',ierr)
         call NhlFCreate(workid,'cn08Work',
     +        NhlFPSWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,count,
     +        ierr)
         call NhlFRLSetString(srlist,'wkPDFFileName','./cn08f.pdf',ierr)
         call NhlFCreate(workid,'cn08Work',
     +        NhlFPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,count,
     +        ierr)
         call NhlFRLSetString(srlist,'wkFileName','./cn08f',ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(workid,'cn08Work',
     +        NhlFCairoPSPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetMDFloatArray(srlist,'wkColorMap',cmap,2,count,
     +        ierr)
         call NhlFRLSetString(srlist,'wkFileName','./cn08f',ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(workid,'cn08Work',
     +        NhlFCairoImageWorkstationClass,0,srlist,ierr)
      endif
C
C  Open data file containing grid of global temperatures.
C
      call gngpat(filename,'data',ierr)
      do 10 i=1,256
         if( filename(i:i).eq.char(0) ) then
            filename(i:i+15)='/cdf/contour.cdf'
            goto 15
         endif
 10   continue
C      
C The second argument to 'ncopn' should be NCNOWRIT, but since we
C can't include 'netcdf.inc', we are using the value '0' instead.
C
 15   ncid = ncopn(filename,0,ierr)
C
C Get the lat/lon/level dimensions.
C
      lat_id = ncdid(ncid,'lat',ierr)
      lon_id = ncdid(ncid,'lon',ierr)
      level_id = ncdid(ncid,'level',ierr)
      call ncdinq(ncid,lat_id,recname,latlen,ierr)
      call ncdinq(ncid,lon_id,recname,lonlen,ierr)
      call ncdinq(ncid,level_id,recname,levellen,ierr)
C
C Read in temperature values and convert from degrees F to degrees K.
C
      temp_id = ncvid(ncid,'T',ierr)
      start(1) = 1
      start(2) = 1
      start(3) = 1
      start(4) = 1
      count(1) = 1
      count(2) = latlen
      count(3) = levellen
      count(4) = 1
      call ncvgt(ncid,temp_id,start,count,temp,ierr)
      call ncagt(ncid,temp_id,'_FillValue',spv,ierr)
      do 30 j = 1,levellen
         do 20 k=1,latlen
            temp(k,j) = (temp(k,j) - 273.15) * 9./5. + 32.
 20      continue
 30   continue
C
C Read in lat/lon/level values.
C
      lat_id = ncvid(ncid,'lat',ierr)
      count(1) = latlen
      call ncvgt(ncid,lat_id,start,count,lat,ierr)

      lon_id = ncvid(ncid,'lon',ierr)
      count(1) = lonlen
      call ncvgt(ncid,lon_id,start,count,lon,ierr)

      level_id = ncvid(ncid,'level',ierr)
      count(1) = levellen
      call ncvgt(ncid,level_id,start,count,level,ierr)
C
C Set up initial scalar field with longitude of temperature data.
C
      count(1) = latlen
      count(2) = levellen
      call NhlFRLClear(srlist)
      call NhlFRLSetMDFloatArray(srlist,'sfDataArray',temp,2,count,
     +     ierr)
      call NhlFRLSetFloat(srlist,'sfMissingValueV',spv,ierr)
      call NhlFRLSetFloat(srlist,'sfXCStartV',lat(1),ierr)
      call NhlFRLSetFloat(srlist,'sfXCEndV',lat(latlen),ierr)
      call NhlFRLSetFloatArray(srlist,'sfXArray',lat,latlen,ierr)
      call NhlFRLSetFloatArray(srlist,'sfYArray',level,levellen,ierr)
      call NhlFCreate(field1,'field1',NhlFscalarFieldClass,appid,srlist,
     +     ierr)
C
C Determine extents of grid
C
      if (lat(1) .lt. lat(latlen)) then
         min_lat = lat(1)
         max_lat = lat(latlen)
      else
         max_lat = lat(1)
         min_lat = lat(latlen)
      endif
      if(level(1) .lt. level(levellen)) then
         min_level = level(1)
         max_level = level(levellen)
      else
         max_level = level(1)
         min_level = level(levellen)
      endif
C
C Create contour using manual spacing.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetFloat(srlist,'vpXF',.2,ierr)
      call NhlFRLSetFloat(srlist,'vpYF',.8,ierr)
      call NhlFRLSetFloat(srlist,'vpWidthF',.6,ierr)
      call NhlFRLSetFloat(srlist,'vpHeightF',.6,ierr)
      call NhlFRLSetString(srlist,'cnFillOn','True',ierr)
      call NhlFRLSetInteger(srlist,'cnScalarFieldData',field1,ierr)
      call NhlFRLSetString(srlist,'cnLevelSelectionMode','ManualLevels',
     +     ierr)
      call NhlFRLSetInteger(srlist,'cnMaxLevelCount',25,ierr)
      call NhlFRLSetFloat(srlist,'cnMinLevelValF',-80.0,ierr)
      call NhlFRLSetFloat(srlist,'cnMaxLevelValF',110.0,ierr)
      call NhlFRLSetFloat(srlist,'cnLevelSpacingF',10.0,ierr)
      call NhlFRLSetFloat(srlist,'trXMinF',min_lat,ierr)
      call NhlFRLSetFloat(srlist,'trXMaxF',max_lat,ierr)
      call NhlFRLSetFloat(srlist,'trYMinF',min_level,ierr)
      call NhlFRLSetFloat(srlist,'trYMaxF',max_level,ierr)
      call NhlFRLSetString(srlist,'trYReverse','True',ierr)
      write(string,55)lon(1)
 55   format('Longitude ',f6.1,' Degrees')
      call NhlFRLSetString(srlist,'tiMainString',string,ierr)
      call NhlFCreate(con1,'con1',NhlFcontourPlotClass,workid,srlist,
     +     ierr)
C
C Draw first step
C
      call NhlFDraw(con1,ierr)
      call NhlFFrame(workid,ierr)
C
C Loop on remaining longitude values and reset the title every iteration.
C
      do 60 i = 2,lonlen
C
C Read in temperature values and convert from degrees F to degrees K.
C
         temp_id = ncvid(ncid,'T',ierr)
         start(1) = i
         start(2) = 1
         start(3) = 1
         start(4) = 1
         count(1) = 1
         count(2) = latlen
         count(3) = levellen
         count(4) = 1
         call ncvgt(ncid,temp_id,start,count,temp,ierr)
         do 70 j = 1,levellen
            do 65 k=1,latlen
               temp(k,j) = (temp(k,j) - 273.15) * 9./5. + 32.
 65         continue
 70      continue
         call NhlFRLClear(srlist)
         count(1) = latlen
         count(2) = levellen
         call NhlFRLSetMDFloatArray(srlist,'sfDataArray',temp,2,count,
     +        ierr)
C
C Create new scalar field.
C
         call NhlFSetValues(field1,srlist,ierr)
         call NhlFRLClear(srlist)
         write(string,75)lon(i)
 75      format('Longitude ',f6.1,' Degrees')
         call NhlFRLSetString(srlist,'tiMainString',string,ierr)
         call NhlFSetValues(con1,srlist,ierr)
         call NhlFDraw(con1,ierr)
         call NhlFFrame(workid,ierr)
 60   continue
C
C Close the netCDF file.
C
      call ncclos(ncid,ierr)
C
C NhlFDestroy destroys the given id and all of its children.
C
      call NhlFRLDestroy(srlist)
      call NhlFDestroy(appid,ierr)
C
C Restores state.
C
      call NhlFClose
      stop
      end

