C
C      $Id: cn07f.f,v 1.5 1997-05-22 16:14:08 haley Exp $
C
C***********************************************************************
C                                                                      *
C                Copyright (C)  1995                                   *
C        University Corporation for Atmospheric Research               *
C     The use of this Software is governed by a License Agreement      *
C                                                                      *
C***********************************************************************
C
C  File:       cn07f.f
C
C  Author:     Ethan Alpert (converted to Fortran by Mary Haley)
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Fri Oct  6 09:48:59 MDT 1995
C
C  Description:    Reads a netCDF file and produces a series of
C                  contour plots.
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFXWorkstationClass
      external NhlFScalarFieldClass
      external NhlFContourPlotClass

      integer xbvalues(6),ylvalues(5)
      character*4 xblabels(6)
      character*3 yllabels(5)
      data xbvalues/-60,-75,-90,-105,-120,-135/
      data ylvalues/60,50,40,30,20/
      data xblabels/'60W','75W','90W','105W','120W','135W'/
      data yllabels/'60N','50N','40N','30N','20N'/

C
C Declare variables for the HLU routine calls.
C
      integer appid, workid, field1, con1
      integer srlist, i
C
C Declare variables for getting information from netCDF file.
C
      integer  ncid, lon_id, lat_id, frtime_id, Z_id, frtime(7)
      real Z(36,33), spv
      real lon(36), lat(33)
      integer start(4), count(4), lonlen, latlen, frtimelen
      character*256 filename
      character*50 recname
C
C Default is to create an NCGM file.
C
      integer NCGM, X11, PS
C
C Default is to create an NCGM file.
C
      NCGM=1
      X11=0
      PS=0
C
C Initialize the HLU library and set up resource template.
C
      call NhlFInitialize
      call NhlFRLCreate(srlist,'setrl')
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(srlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'cn07',NhlFAppClass,0,srlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkMetaName','./cn07f.ncgm',ierr)
         call NhlFCreate(workid,'cn07Work',
     +        NhlFNcgmWorkstationClass,0,srlist,ierr)
      else if (X11.eq.1) then
C
C Create an xworkstation object.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPause','True',ierr)
         call NhlFCreate(workid,'cn07Work',NhlFXWorkstationClass,
     +        0,srlist,ierr)
      else if (PS.eq.1) then
C
C Create a PostScript workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPSFileName','./cn07f.ps',ierr)
         call NhlFCreate(workid,'cn07Work',
     +        NhlFPSWorkstationClass,0,srlist,ierr)
      endif
C
C Open NetCDF file containing Geo-Potential height forecast
C information.
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
C Get the lat/lon dimensions.
C
      lat_id = ncdid(ncid,'lat',ierr)
      lon_id = ncdid(ncid,'lon',ierr)
      frtime_id = ncdid(ncid,'frtime',ierr)
      call ncdinq(ncid,lat_id,recname,latlen,ierr)
      call ncdinq(ncid,lon_id,recname,lonlen,ierr)
      call ncdinq(ncid,frtime_id,recname,frtimelen,ierr)
      frtime_id = ncvid(ncid,'frtime',ierr)
C
C Read in Z.
C
      Z_id = ncvid(ncid,'Z',ierr)
      start(1) = 1
      start(2) = 1
      start(3) = 4
      start(4) = 1
      count(1) = lonlen
      count(2) = latlen
      count(3) = 1
      count(4) = 1
      call ncvgt(ncid,Z_id,start,count,Z,ierr)
      call ncagt(ncid,Z_id,'_FillValue',spv,ierr)
C
C Read in lat/lon/frtime values.
C
      lat_id = ncvid(ncid,'lat',ierr)
      count(1) = latlen
      call ncvgt(ncid,lat_id,start,count,lat,ierr)

      lon_id = ncvid(ncid,'lon',ierr)
      count(1) = lonlen
      call ncvgt(ncid,lon_id,start,count,lon,ierr)

      frtime_id = ncvid(ncid,'frtime',ierr)
      count(1) = frtimelen
      call ncvgt(ncid,frtime_id,start,count,frtime,ierr)
C
C Create a scalar field object and configure the missing values and
C the start and end information.
C
      count(1) = lonlen
      count(2) = latlen
      call NhlFRLClear(srlist)
      call NhlFRLSetMDFloatArray(srlist,'sfDataArray',Z,2,count,
     +     ierr)
      call NhlFRLSetFloat(srlist,'sfMissingValueV',spv,ierr)
      call NhlFRLSetFloat(srlist,'sfXCStartV',lon(1),ierr)
      call NhlFRLSetFloat(srlist,'sfXCEndV',lon(lonlen),ierr)
      call NhlFRLSetFloat(srlist,'sfYCStartV',lat(1),ierr)
      call NhlFRLSetFloat(srlist,'sfYCEndV',lat(latlen),ierr)
      call NhlFCreate(field1,'field1',NhlFscalarFieldClass,appid,srlist,
     +     ierr)
C
C Create contour object.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetFloat(srlist,'vpXF',.2,ierr)
      call NhlFRLSetFloat(srlist,'vpYF',.8,ierr)
      call NhlFRLSetFloat(srlist,'vpWidthF',.6,ierr)
      call NhlFRLSetFloat(srlist,'vpHeightF',.6,ierr)
      call NhlFRLSetInteger(srlist,'cnScalarFieldData',field1,ierr)
      call NhlFRLSetString(srlist,'cnLevelSelectionMode','ManualLevels',
     +     ierr)
      call NhlFRLSetFloat(srlist,'cnMinLevelValF',5400.0,ierr)
      call NhlFRLSetFloat(srlist,'cnMaxLevelValF',5950.0,ierr)
      call NhlFRLSetFloat(srlist,'cnLevelSpacingF',50.0,ierr)
      call NhlFRLSetString(srlist,'cnFillOn','True',ierr)
      call NhlFRLSetFloat(srlist,'trXMinF',-140.0,ierr)
      call NhlFRLSetFloat(srlist,'trXMaxF',-52.5,ierr)
      call NhlFRLSetFloat(srlist,'trYMinF',20.0,ierr)
      call NhlFRLSetFloat(srlist,'trYMaxF',60.0,ierr)
      call NhlFRLSetString(srlist,'tiMainString',
     +     'Geo-potential height @500mb',ierr)
      call NhlFRLSetString(srlist,'tiXAxisString','Lon',ierr)
      call NhlFRLSetString(srlist,'tiYAxisString','Lat',ierr)
      call NhlFRLSetString(srlist,'tmXBMode','EXPLICIT',ierr)
      call NhlFRLSetIntegerArray(srlist,'tmXBValues',xbvalues,6,ierr)
      call NhlFRLSetStringArray(srlist,'tmXBLabels',xblabels,6,ierr)
      call NhlFRLSetString(srlist,'tmYLMode','EXPLICIT',ierr)
      call NhlFRLSetIntegerArray(srlist,'tmYLValues',ylvalues,5,ierr)
      call NhlFRLSetStringArray(srlist,'tmYLLabels',yllabels,5,ierr)
      call NhlFRLSetString(srlist,'tmXMajorGrid','True',ierr)
      call NhlFRLSetString(srlist,'tmYMajorGrid','True',ierr)
      call NhlFRLSetString(srlist,'tmXBMinorOn','False',ierr)
      call NhlFRLSetString(srlist,'tmYLMinorOn','False',ierr)
      call NhlFCreate(con1,'con1',NhlFcontourPlotClass,workid,srlist,
     +     ierr)
C
C Draw first frame
C
      call NhlFDraw(con1,ierr)
      call NhlFFrame(workid,ierr)
C
C Loop on remaining time steps.
C
      do 60 i = 2,frtimelen
C
C Read in new section of Z.
C
         start(1) = 1
         start(2) = 1
         start(3) = 4
         start(4) = i
         count(1) = lonlen
         count(2) = latlen
         count(3) = 1
         count(4) = 1
         call ncvgt(ncid,Z_id,start,count,Z,ierr)
C
C Create new scalar field.
C
         call NhlFRLClear(srlist)
         count(1) = lonlen
         count(2) = latlen
         call NhlFRLSetMDFloatArray(srlist,'sfDataArray',Z,2,count,
     +        ierr)
         call NhlFSetValues(field1,srlist,ierr)
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

