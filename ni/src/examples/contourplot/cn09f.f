C
C      $Id: cn09f.f,v 1.1 1995-09-28 16:36:37 haley Exp $
C
C***********************************************************************
C                                                                      *
C                Copyright (C)  1995                                   *
C        University Corporation for Atmospheric Research               *
C     The use of this Software is governed by a License Agreement      *
C                                                                      *
C***********************************************************************
C
C  File:       cn09f.f
C
C  Author:     Ethan Alpert (converted to Fortran by Mary Haley)
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Thu Sep 28 09:59:38 MDT 1995
C
C  Description:    Reads a netCDF file and produces a series of
C                  surface pressure contour plots.
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFXWorkstationClass
      external NhlFScalarFieldClass
      external NhlFContourPlotClass
C
C Declare variables for the HLU routine calls.
C
      integer appid, workid, field1, con1
      integer srlist, i, j, k
C
C Declare variables for getting information from netCDF file.
C
      integer  ncid, lon_id, lat_id, frtime_id, press_id, frtime(7)
      real press(36,33), spv
      real lon(36), lat(33)
      integer start(3), count(3), lonlen, latlen, frtimelen
      character*256 filename
      character*20 string
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
      call NhlFCreate(appid,'cn09',NhlFAppClass,0,srlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkMetaName','./cn09f.ncgm',ierr)
         call NhlFCreate(workid,'cn09Work',
     +        NhlFNcgmWorkstationClass,0,srlist,ierr)
      else if (X11.eq.1) then
C
C Create an xworkstation object.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPause','True',ierr)
         call NhlFCreate(workid,'cn09Work',NhlFXWorkstationClass,
     +        0,srlist,ierr)
      else if (PS.eq.1) then
C
C Create a PostScript workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPSFileName','./cn09f.ps',ierr)
         call NhlFCreate(workid,'cn09Work',
     +        NhlFPSWorkstationClass,0,srlist,ierr)
      endif
C
C Open data file containing surface pressure data for
C entire globe.
C
      call gngpat(filename,'data',ierr)
      flen = 16
      do 10 i=1,256
         if( filename(i:i).eq.char(0) ) then
            filename(i:i+flen)='/cdf/contour.cdf'
            goto 15
         endif
 10   continue
      
 15   ncid = ncopn(filename,NCNOWRIT,ierr)
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
C Read in surface pressure and convert it to millibars.
C
      press_id = ncvid(ncid,'Psl',ierr)
      start(1) = 1
      start(2) = 1
      start(3) = 1
      count(1) = lonlen
      count(2) = latlen
      count(3) = 1
      call ncvgt(ncid,press_id,start,count,press,ierr)
      call ncagt(ncid,press_id,'_FillValue',spv,ierr)
      do 30 j = 1,latlen
         do 20 k=1,lonlen
            press(k,j) = press(k,j)/100.
 20      continue
 30   continue
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
C Create scalar field configured with first time step
C of pressure data.
C
      count(1) = lonlen
      count(2) = latlen
      call NhlFRLClear(srlist)
      call NhlFRLSetMDFloatArray(srlist,'sfDataArray',press,2,count,
     +     ierr)
      call NhlFRLSetFloat(srlist,'sfMissingValueV',spv,ierr)
      call NhlFRLSetFloat(srlist,'sfXCStartV',lon(1),ierr)
      call NhlFRLSetFloat(srlist,'sfXCEndV',lon(lonlen),ierr)
      call NhlFRLSetFloat(srlist,'sfYCStartV',lat(1),ierr)
      call NhlFRLSetFloat(srlist,'sfYCEndV',lat(latlen),ierr)
      call NhlFCreate(field1,'field1',NhlFscalarFieldClass,appid,srlist,
     +     ierr)
C
C Create contour object using manual level spacing and solid
C color fill
C
      call NhlFRLClear(srlist)
      call NhlFRLSetInteger(srlist,'cnScalarFieldData',field1,ierr)
      call NhlFRLSetString(srlist,'cnLevelSelectionMode','ManualLevels',
     +     ierr)
      call NhlFRLSetFloat(srlist,'cnMinLevelValF',960.0,ierr)
      call NhlFRLSetFloat(srlist,'cnMaxLevelValF',1040.0,ierr)
      call NhlFRLSetFloat(srlist,'cnLevelSpacingF',5.0,ierr)
      call NhlFRLSetInteger(srlist,'cnMaxLevelCount',25,ierr)
      call NhlFRLSetString(srlist,'cnInfoLabelOn','OFF',ierr)
      call NhlFRLSetString(srlist,'cnHighLabelsOn','OFF',ierr)
      call NhlFRLSetString(srlist,'cnLowLabelsOn','OFF',ierr)
      call NhlFRLSetString(srlist,'cnLineLabelsOn','False',ierr)
      call NhlFRLSetString(srlist,'cnLinesOn','False',ierr)
      call NhlFRLSetString(srlist,'cnFillOn','True',ierr)
      call NhlFRLSetString(srlist,'tiMainString','Forecast Time 0',ierr)
      call NhlFCreate(con1,'con1',NhlFcontourPlotClass,workid,srlist,
     +     ierr)
C
C Draw first frame
C
      call NhlFDraw(con1,ierr)
      call NhlFFrame(workid,ierr)
C
C Loop and draw remaining frames reseting the scalar field object
C with a new array for each iteration
C
      do 60 i = 2,frtimelen
C
C Read in surface pressure and convert it to millibars.
C
         start(1) = 1
         start(2) = 1
         start(3) = i
         count(1) = lonlen
         count(2) = latlen
         count(3) = 1
         call ncvgt(ncid,press_id,start,count,press,ierr)
         do 50 j = 1,latlen
            do 40 k=1,lonlen
               press(k,j) = press(k,j)/100.
 40         continue
 50      continue
         call NhlFRLClear(srlist)
         count(1) = lonlen
         count(2) = latlen
         call NhlFRLSetMDFloatArray(srlist,'sfDataArray',press,2,count,
     +        ierr)
C
C Create new scalar field.
C
         call NhlFSetValues(field1,srlist,ierr)
         call NhlFRLClear(srlist)
         write(string,55)i
 55      format('Forecast Time ',i2)
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

