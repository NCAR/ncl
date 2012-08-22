C
C      $Id: st04f.f,v 1.10 2010-03-15 22:49:24 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1996                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       st04f.f
C
C  Author:     David Brown (converted by Mary Haley)
C              National Center for Atmospheric Research
C              PO 3000, Boulder, Colorado
C
C  Date:       Thu June 27 16:25:31 MST 1996
C
C  Description: This example shows a StreamlinePlot of 500 mb wind 
C               vector data overlaid on a MapPlot. The streamlines
C               are drawn over a VectorPlot of surface winds colored
C               by surface pressure that in turn is drawn over a filled
C               ContourPlot of surface temperature. Different intervals
C               of the "temp1" colormap are used to color the contour
C               levels and the vectors.
C               The data represents 15 days of weather over North
C               America in January, 1996.
C               The data is extracted from NMC forcast data produced 
C               at 12 hour intervals and converted to netcdf format 
C               by Unidata. Most of the time steps in the files
C               extracted from the original data are taken from the 
C               0 and 6 hour forecast times. However, because some of
C               the original files were lost, certain time steps come
C               from longer range forcasts. Also, several steps had to
C               be excluded from the frame set because the data is 
C               defective. The result is that there is an 
C               apparent discontinuity between some of the frames 
C               when the output is animated.
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFVectorFieldClass
      external NhlFVectorPlotClass
      external NhlFScalarFieldClass
      external NhlFStreamlinePlotClass
      external NhlFContourPlotClass
      external NhlFTextItemClass
      external NhlFMapPlotClass
C
C Depending on the value of the ITIMESTEPS variable declared below,
C this example example can generate up to 61 frames from the 64
C timesteps in the data files. As shipped, only the first 20 frames 
C are created. To see the complete plot uncomment the second 
C assignment to ITIMESTEPS. Some systems may not have enough physical
C memory to allow all frames to be viewed as an animation.
C
C      parameter(ITIMESTEPS=64)
C
      parameter(ITIMESTEPS=20)
      parameter(NLAT=33,NLON=36)

      character*7  wks_type
      integer i, j, k, d, h
      integer appid, wid, cnid, vcid, stid, txid, amid, mpid, tmid
      integer vfield, vfield2, sfield, sfield2
      integer rlist, len_dims(2)
      integer latlen, lonlen
      integer timelen, timestep(64)
      integer strt(1), cnt(1)
      integer ncid(6), uid, vid, u5id, v5id, pid, tid
      integer latid,lonid
      real lon(NLON), lat(NLAT)
      real X(NLON,NLAT), Y(NLON,NLAT)
      character*2 hour, day
      character*16 mainstring
      character*256 filename
      character*256 dir
      character*8 rftime
      character*50 recname
      character*13 cdffiles(6)
      integer cdflens(6)
      data cdffiles/'Ustorm.cdf','Vstorm.cdf','Pstorm.cdf',
     +              'Tstorm.cdf','U500storm.cdf','V500storm.cdf'/
      data cdflens/10,10,10,10,13,13/
      integer flen

C
C Define the workstation type
C
      wks_type = "ncgm"

C
C Initialize the high level utility library
C
      call NhlFInitialize
C
C Create an application context. Set the app dir to the current
C directory so the application looks for a resource file in the working
C directory. 
C
      call NhlFRLCreate(rlist,'setrl')

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFRLSetString(rlist,'appDefaultParent','True',ierr)
      call NhlFCreate(appid,'st04',NhlFappClass,0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName','./st04f.ncgm',ierr)
         call NhlFRLSetString(rlist,'wkColorMap','temp1',ierr)
         call NhlFCreate(wid,'st04Work',
     +        NhlFNcgmWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPause','True',ierr)
         call NhlFRLSetString(rlist,'wkColorMap','temp1',ierr)
         call NhlFCreate(wid,'st04Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,rlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkColorMap','temp1',ierr)
         call NhlFRLSetString(rlist,'wkPSFileName','./st04f.ps',ierr)
         call NhlFCreate(wid,'st04Work',
     +        NhlFPSWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkColorMap','temp1',ierr)
         call NhlFRLSetString(rlist,'wkPDFFileName','./st04f.pdf',ierr)
         call NhlFCreate(wid,'st04Work',
     +        NhlFPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkColorMap','temp1',ierr)
         call NhlFRLSetString(rlist,'wkFileName','./st04f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'st04Work',
     +        NhlFCairoPSPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkColorMap','temp1',ierr)
         call NhlFRLSetString(rlist,'wkFileName','./st04f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'st04Work',
     +        NhlFCairoImageWorkstationClass,0,rlist,ierr)
      endif
C
C Open the netCDF files.
C
      call gngpat(dir,'data',ierr)
      flen = 0
      do 10 i=1,256
          if( dir(i:i).eq.char(0) ) then
              dir(i:i+4) = '/cdf/'
              flen = i+4
              go to 15
          endif
 10   continue
 15   filename(1:flen) = dir
      do 20 j = 1,6
          filename(flen+1:flen+cdflens(j)) = cdffiles(j)
          ncid(j) = ncopn(filename(1:flen+cdflens(j)),0,ierr)
 20   continue
C      
C The second argument to 'ncopn' should be NCNOWRIT, but since we
C can't include 'netcdf.inc', we are using the value '0' instead.
C
C
C Get the lat/lon dimensions (they happen to be the
C same for all files in this case)
C
      latid = ncdid(ncid(1),'lat',ierr)
      lonid = ncdid(ncid(1),'lon',ierr)
      call ncdinq(ncid(1),latid,recname,latlen,ierr)
      call ncdinq(ncid(1),lonid,recname,lonlen,ierr)
      len_dims(1) = lonlen
      len_dims(2) = latlen
C
C Get the variable ids
C
      uid = ncvid(ncid(1),'u',ierr)
      vid = ncvid(ncid(2),'v',ierr)
      pid = ncvid(ncid(3),'p',ierr)
      tid = ncvid(ncid(4),'t',ierr)
      u5id = ncvid(ncid(5),'u',ierr)
      v5id = ncvid(ncid(6),'v',ierr)
      latid = ncvid(ncid(1),'lat',ierr)
      lonid = ncvid(ncid(1),'lon',ierr)
C
C Get lat/lon values (they are the same for all files)
C
      strt(1) = 1
      cnt(1) = latlen
      call ncvgt(ncid(1),latid,strt,cnt,lat,ierr)
      cnt(1) = lonlen
      call ncvgt(ncid(1),lonid,strt,cnt,lon,ierr)
C
C Get U and V data values
C
      call get_2d_array(X,latlen,lonlen,ncid(1),uid,1)
      call get_2d_array(Y,latlen,lonlen,ncid(2),vid,1)
C
C Create a VectorField of the surface wind data
C
      call NhlFRLClear(rlist)
      call NhlFRLSetMDFloatArray(rlist,'vfUDataArray',X,2,len_dims,ierr)
      call NhlFRLSetMDFloatArray(rlist,'vfVDataArray',Y,2,len_dims,ierr)
      call NhlFRLSetFloat(rlist,'vfXCStartV',lon(1),ierr)
      call NhlFRLSetFloat(rlist,'vfYCStartV',lat(1),ierr)
      call NhlFRLSetFloat(rlist,'vfXCEndV', lon(lonlen),ierr)
      call NhlFRLSetFloat(rlist,'vfYCEndV',lat(latlen),ierr)
      call NhlFRLSetFloat(rlist,'vfMissingUValueV',-9999.0,ierr)
      call NhlFCreate(vfield,'VectorField',NhlFvectorFieldClass,appid,
     +     rlist,ierr)
C
C Create a VectorField of 500 millibar wind data
C
C Get U and V values
C
      call get_2d_array(X,latlen,lonlen,ncid(5),u5id,1)
      call get_2d_array(Y,latlen,lonlen,ncid(6),v5id,1)

      call NhlFRLClear(rlist)
      call NhlFRLSetMDFloatArray(rlist,'vfUDataArray',X,2,len_dims,
     +     ierr)
      call NhlFRLSetMDFloatArray(rlist,'vfVDataArray',Y,2,len_dims,
     +     ierr)
      call NhlFRLSetFloat(rlist,'vfXCStartV',lon(1),ierr)
      call NhlFRLSetFloat(rlist,'vfYCStartV',lat(1),ierr)
      call NhlFRLSetFloat(rlist,'vfXCEndV', lon(lonlen),ierr)
      call NhlFRLSetFloat(rlist,'vfYCEndV',lat(latlen),ierr)
      call NhlFRLSetFloat(rlist,'vfMissingUValueV',-9999.0,ierr)
      call NhlFCreate(vfield2,'VectorField',NhlFvectorFieldClass,appid,
     +     rlist,ierr)
C
C Create a ScalarField of surface pressure 
C
C Get P data values
C
      call get_2d_array(X,latlen,lonlen,ncid(3),pid,1)

      do 41 j = 1, latlen
         do 40 i = 1, lonlen
            if( X(i,j) .ne. -9999.0 ) then
               X(i,j) = X(i,j) / 100.
            endif
 40      continue
 41   continue
      
      call NhlFRLClear(rlist)
      call NhlFRLSetMDFloatArray(rlist,'sfDataArray',X,2,len_dims,ierr)
      call NhlFRLSetFloat(rlist,'sfXCStartV',lon(1),ierr)
      call NhlFRLSetFloat(rlist,'sfYCStartV',lat(1),ierr)
      call NhlFRLSetFloat(rlist,'sfXCEndV',lon(lonlen),ierr)
      call NhlFRLSetFloat(rlist,'sfYCEndV',lat(latlen),ierr)
      call NhlFRLSetFloat(rlist,'sfMissingValueV', -9999.0,ierr)
      call NhlFCreate(sfield,'ScalarField',NhlFscalarFieldClass,appid,
     +     rlist,ierr)
C
C Create a ScalarField of surface temperature 
C (convert from Kelvin to Farenheit)
C
C Get T data values
C
      call get_2d_array(X,latlen,lonlen,ncid(4),tid,1)
C
C Convert to Fahrenheit
C
      do 50 j = 1, latlen
         do 49 i = 1, lonlen
            if( X(i,j) .ne. -9999.0) then
               X(i,j) = (X(i,j) - 273.15) * 9.0/5.0 + 32.0
            endif
 49   continue
 50   continue
      
      call NhlFRLClear(rlist)
      call NhlFRLSetMDFloatArray(rlist,'sfDataArray',X,2,len_dims,ierr)
      call NhlFRLSetFloat(rlist,'sfXCStartV',lon(1),ierr)
      call NhlFRLSetFloat(rlist,'sfYCStartV',lat(1),ierr)
      call NhlFRLSetFloat(rlist,'sfXCEndV',lon(lonlen),ierr)
      call NhlFRLSetFloat(rlist,'sfYCEndV',lat(latlen),ierr)
      call NhlFRLSetFloat(rlist,'sfMissingValueV', -9999.0,ierr)
      call NhlFCreate(sfield2,'ScalarField2',NhlFscalarFieldClass,appid,
     +     rlist,ierr)
C
C Create a ContourPlot with surface temperature data
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'cnFillOn','true',ierr)
      call NhlFRLSetString(rlist,'cnLinesOn','false',ierr)
      call NhlFRLSetString(rlist,'cnFillDrawOrder','predraw',ierr)
      call NhlFRLSetInteger(rlist,'cnScalarFieldData',sfield2,ierr)
      call NhlFCreate(cnid,'contourplot',NhlFcontourPlotClass,wid,
     +     rlist,ierr)
C
C Create a VectorPlot with the surface wind and pressure data
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'vcUseScalarArray','true',ierr)
      call NhlFRLSetInteger(rlist,'vcVectorFieldData',vfield,ierr)
      call NhlFRLSetInteger(rlist,'vcScalarFieldData',sfield,ierr)
      call NhlFCreate(vcid,'vectorplot',NhlFvectorPlotClass,wid,rlist,
     +     ierr)
C
C Create a StreamlinePlot with 500 mb wind data
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'pmTitleDisplayMode','always',ierr)
      call NhlFRLSetString(rlist,'tiMainFuncCode','~',ierr)
      call NhlFRLSetInteger(rlist,'stVectorFieldData',vfield2,ierr)
      call NhlFCreate(stid,'streamlineplot',NhlFstreamlinePlotClass,
     +     wid,rlist,ierr)
C
C Create an annotation used to explain the streamline data
C
      call NhlFCreate(txid,'streamlineplotanno',NhlFtextItemClass,wid,0,
     +     ierr)
      call NhlFAddAnnotation(stid,txid,amid)
C
C Create a map object
C
      call NhlFRLClear(rlist)
C      call NhlFRLSetString(rlist,'vpUseSegments','true',ierr)
      call NhlFCreate(mpid,'mapplot',NhlFmapPlotClass,wid,rlist,ierr)
C
C Overlay everything on the MapPlot. The last object overlaid will
C appear on top
C
      call NhlFAddOverlay(mpid,cnid,-1,ierr)
      call NhlFAddOverlay(mpid,vcid,-1,ierr)
      call NhlFAddOverlay(mpid,stid,-1,ierr)
C
C Variables for manipulating the title string
C
      tmid = ncdid(ncid(2),'timestep',ierr)
      call ncdinq(ncid(2),tmid,recname,timelen,ierr)
      tmid = ncvid(ncid(2),'timestep',ierr)

      strt(1) = 1
      cnt(1) = timelen
      call ncvgt(ncid(2),tmid,strt,cnt,timestep,ierr)
      hour = '00'
      day  = '05'
    
      tmid = ncvid(ncid(2),'reftime',ierr)
      strt(1) = 1 
      cnt(1) = 8
      call ncvgtc(ncid(2),tmid,strt,cnt,rftime,8,ierr)

      do 100 i=0,ITIMESTEPS-1
         if (i .ne. 17 .and. i .ne. 36 .and. i .ne. 37) then
C
C Figure out the hour and day from the timestep, convert to strings
C and build the title string
C
            d = timestep(i+1) / 24 + 5
            h = mod(timestep(i+1),24)
            if (h .gt. 9) then
               write(hour,54)h
            else
               write(hour,55)h
            endif
            if (d .gt. 9) then
               write(day,54)d
            else
               write(day,55)d
            endif
 54         format(I2)
 55         format('0',I1)
C     
C Set the new title string
C     
            mainstring = rftime // day // ' ' // hour // ':00'
            print *,mainstring
            call NhlFRLClear(rlist)
            call NhlFRLSetString(rlist,'tiMainString',mainstring,ierr)
            call NhlFSetValues(stid,rlist,ierr)
C
C Modify the data objects with data for the current time step
C
C Get U and V values
C
            call get_2d_array(X,latlen,lonlen,ncid(1),uid,i+1)
            call get_2d_array(Y,latlen,lonlen,ncid(2),vid,i+1)

            call NhlFRLClear(rlist)
            call NhlFRLSetMDFloatArray(rlist,'vfUDataArray',X,2,
     +           len_dims,ierr)
            call NhlFRLSetMDFloatArray(rlist,'vfVDataArray',Y,2,
     +           len_dims,ierr)
            call NhlFSetValues(vfield,rlist,ierr)
C
C Get U and V values
C
            call get_2d_array(X,latlen,lonlen,ncid(5),uid,i+1)
            call get_2d_array(Y,latlen,lonlen,ncid(6),vid,i+1)

            call NhlFRLClear(rlist)
            call NhlFRLSetMDFloatArray(rlist,'vfUDataArray',X,2,
     +           len_dims,ierr)
            call NhlFRLSetMDFloatArray(rlist,'vfVDataArray',Y,2,
     +           len_dims,ierr)
            call NhlFSetValues(vfield2,rlist,ierr)
C
C Get P values
C
            call get_2d_array(X,latlen,lonlen,ncid(3),pid,i+1)

            do 70 j = 1, latlen
               do 60 k = 1, lonlen
                  if( X(k,j) .ne. -9999.0 ) then
                     X(k,j) = X(k,j) / 100.
                  endif
 60            continue
 70         continue

            call NhlFRLClear(rlist)
            call NhlFRLSetMDFloatArray(rlist,'sfDataArray',X,2,len_dims,
     +           ierr)
            call NhlFSetValues(sfield,rlist,ierr)
C
C Get T values
C
            call get_2d_array(X,latlen,lonlen,ncid(4),tid,i+1)
C
C Convert to Fahrenheit
C
            do 90 j = 1, latlen
               do 80 k = 1, lonlen
                  if( X(k,j) .ne. -9999.0) then
                     X(k,j) = (X(k,j) - 273.15) * 9.0/5.0 + 32.0
                  endif
 80            continue
 90         continue
      
            call NhlFRLClear(rlist)
            call NhlFRLSetMDFloatArray(rlist,'sfDataArray',X,2,len_dims,
     +           ierr)
            call NhlFSetValues(sfield2,rlist,ierr)
C
C Draw the plot
C
            call NhlFDraw(mpid,ierr)
            call NhlFFrame(wid,ierr)
         endif
 100  continue
C 
C  Destroy the workstation object and exit.
C
      call NhlFDestroy(wid,ierr)
      call NhlFClose
      stop
      end

      subroutine get_2d_array(X,latlen,lonlen,fid,aid,timestep)
      real X(lonlen,latlen)
      integer latlen, lonlen, fid, aid, timestep
      integer start(3), count(3)

      start(1) = 1
      start(2) = 1
      start(3) = timestep
      count(1) = lonlen
      count(2) = latlen
      count(3) = 1
      call ncvgt(fid,aid,start,count,X,ierr)
      return
      end


