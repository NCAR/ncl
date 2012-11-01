C
C  $Id: vc09f.f,v 1.7 2010-03-15 22:49:25 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1997                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   File:       vc09f.f
C   Author:     David Brown
C               National Center for Atmospheric Research
C               PO 3000, Boulder, Colorado
C
C               Converted to Fortran by Scott Snodgrass
C
C   Date:       Wed Jul  9 08:00:28 MDT 1997
C
C   Description:    Does an animation of the January 1996 snow storm.
C                   Wind vectors colored by temperature are animated
C                   over a pressure field contour plot.
C

      external NhlFAppClass
      external NhlFTitleClass
      external NhlFTextItemClass
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
      external NhlFMapPlotClass

      parameter(TIMESTEPS=64)
      parameter(NLAT=33,NLON=36)

      integer ZOOM
      integer appid, wid, vfield, sfield, sfield2
      integer mapid, cnid, vcid, tiid1, tiid2,txid1
      integer u_id, v_id, p_id, t_id, lat_id, lon_id, titl_id, tim_id
      integer uf, vf, pf, tf, latlen, lonlen, timlen, flen
      integer start(3), count (3), len_dims (2), timestep (64)
      character*7  wks_type

      real lon (NLON), lat(NLAT)
      real U(NLON,NLAT), V(NLON,NLAT), P(NLON,NLAT), T(NLON,NLAT)
      real MinLat, MaxLat, MinLon, MaxLon, tm

      character*3 day
      character*256 mainstring
      character*256 dir
      character*256 filename
      character*8 reftime
      character*50 recname
      character*10 Uname, Vname, Pname, Tname
      data Uname /'Ustorm.cdf'/
      data Vname /'Vstorm.cdf'/
      data Pname /'Pstorm.cdf'/
      data Tname /'Tstorm.cdf'/

C
C If zoom = 0 then this script will animate the original map.
C If zoom = 1 then this script will animate a zoomed map.
C

      ZOOM = 0

C
C Output to an ncgm.
C
      wks_type = "ncgm"

      call NhlFInitialize
      call NhlFRLCreate (rlist,  'setrl')

C
C Create an application object.  It will look for a resource file
C named vc09.res.
C

      call NhlFRLClear (rlist)
      call NhlFRLSetString (rlist, 'appUsrDir', './', ierr)
      call NhlFRLSetString (rlist, 'appDefaultParent', 'True', ierr)
      call NhlFCreate (appid, 'vc09', NhlFappClass, 0, rlist, ierr)

C
C Create an ncgmWorkstation object.
C
      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
         call NhlFRLClear (rlist)
         call NhlFRLSetString (rlist, 'wkMetaName', './vc09f.ncgm', 
     +        ierr)
         call NhlFRLSetString (rlist, 'wkColorMap', 'temp1', ierr)
         call NhlFCreate (wid, 'vc09Work', NhlFNcgmWorkstationClass, 0,
     +        rlist, ierr)

C
C Create an XWorkstation object.
C
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
         call NhlFRLClear (rlist)
         call NhlFRLSetString (rlist, 'wkPause', 'True', ierr)
         call NhlFRLSetString (rlist, 'wkColorMap', 'temp1', ierr)
         call NhlFCreate (wid, 'vc09Work', 
     +        NhlFCairoWindowWorkstationClass, 0,
     +        rlist, ierr)

C
C Create a PSWorkstation object.
C
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
         call NhlFRLClear (rlist)
         call NhlFRLSetString (rlist, 'wkPSFileName', 'vc09f.ps', ierr)
         call NhlFRLSetString (rlist, 'wkColorMap', 'temp1', ierr)
         call NhlFCreate (wid, 'vc09Work', NhlFPSWorkstationClass, 0,
     +        rlist,  ierr)
C
C Create a PDF Workstation object.
C
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
         call NhlFRLClear (rlist)
         call NhlFRLSetString (rlist, 'wkPDFFileName', 'vc09f.pdf',
     +        ierr)
         call NhlFRLSetString (rlist, 'wkColorMap', 'temp1', ierr)
         call NhlFCreate (wid, 'vc09Work', NhlFPDFWorkstationClass, 0,
     +        rlist, ierr)
C
C Create a cairo PS/PDF Workstation object.
C
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
         call NhlFRLClear (rlist)
         call NhlFRLSetString (rlist, 'wkFileName', 'vc09f',
     +        ierr)
         call NhlFRLSetString (rlist, 'wkFormat', wks_type,
     +        ierr)
         call NhlFRLSetString (rlist, 'wkColorMap', 'temp1', ierr)
         call NhlFCreate (wid, 'vc09Work',
     +        NhlFCairoPSPDFWorkstationClass, 0, rlist, ierr)
C
C Create a cairo PNG Workstation object.
C
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
         call NhlFRLClear (rlist)
         call NhlFRLSetString (rlist, 'wkFileName', 'vc09f',
     +        ierr)
         call NhlFRLSetString (rlist, 'wkFormat', wks_type,
     +        ierr)
         call NhlFRLSetString (rlist, 'wkColorMap', 'temp1', ierr)
         call NhlFCreate (wid, 'vc09Work',
     +        NhlFCairoImageWorkstationClass, 0, rlist, ierr)
      end if

      call gngpat (dir, 'data', ierr)
      flen = 0
      do 10 i=1,256
          if( dir(i:i).eq.char(0) ) then
              dir(i:i+4) = '/cdf/'
              flen = i+4
              go to 15
          endif
 10   continue
 15   filename(1:flen)=dir

      filename(flen+1:flen+10) = Uname
      filename(flen+11:flen+11) = char(0)
      uf = ncopn (filename, 0, ierr)
      filename(flen+1:flen+10) = Vname
      vf = ncopn (filename, 0, ierr)
      filename(flen+1:flen+10) = Pname
      pf = ncopn (filename, 0, ierr)
      filename(flen+1:flen+10) = Tname
      tf = ncopn (filename, 0, ierr)

C
C Get the lat/lon dimensions (they happen to be the
C same for all files in this case)
C

      lat_id = ncdid (uf, 'lat', ierr)
      lon_id = ncdid (uf, 'lon', ierr)
      call ncdinq(uf, lat_id, recname, latlen, ierr)
      call ncdinq(uf, lon_id, recname, lonlen, ierr)
      len_dims(1) = lonlen
      len_dims(2) = latlen

C
C Get the variable ids
C

      u_id = ncvid (uf, 'u', ierr)
      v_id = ncvid (vf, 'v', ierr)
      p_id = ncvid (pf, 'p', ierr)
      t_id = ncvid (tf, 't', ierr)
      lat_id = ncvid (uf, 'lat', ierr)
      lon_id = ncvid (uf, 'lon', ierr)

C
C Get lat/lon values (they are the same for all files)
C

      start(1) = 1
      start(2) = 1
      start(3) = 1
      count(1) = latlen
      count(2) = 1
      count(3) = 1
      call ncvgt (uf, lat_id, start, count, lat, ierr)
      count(1) = lonlen
      call ncvgt (uf, lon_id, start, count, lon, ierr)

C
C Variables for manipulating the title string
C

      tim_id = ncdid (vf, 'timestep', ierr)
      call ncdinq (vf, tim_id, recname, timlen, ierr)
      tim_id = ncvid (vf, 'timestep', ierr)

      start(1) = 1
      count(1) = timlen
      count(2) = 1
      count(3) = 1
      call ncvgt (vf, tim_id, start, count, timestep, ierr)

      titl_id = ncvid(vf, 'reftime', ierr)
      start(1) = 1
      count(1) = 8
      call ncvgtc(vf, titl_id, start, count, reftime, 8, ierr)

C
C Get U and V data values
C

      call get_2d_array (U, latlen, lonlen, uf, u_id, 1)
      call get_2d_array (V, latlen, lonlen, vf, v_id, 1)
      call get_2d_array (P, latlen, lonlen, pf, p_id, 1)
      call get_2d_array (T, latlen, lonlen, tf, t_id, 1)

      call NhlFRLClear (rlist)
      call NhlFRLSetMDFloatArray (rlist, 'vfUDataArray', U, 2,
     +     len_dims, ierr)
      call NhlFRLSetMDFloatArray (rlist, 'vfVDataArray', V, 2,
     +     len_dims, ierr)
      call NhlFRLSetFloat (rlist, 'vfXCStartV', lon (1), ierr)
      call NhlFRLSetFloat (rlist, 'vfYCStartV', lat (1), ierr)
      call NhlFRLSetFloat (rlist, 'vfXCEndV', lon (lonlen), ierr)
      call NhlFRLSetFloat (rlist, 'vfYCEndV', lat (latlen), ierr)
      call NhlFRLSetInteger (rlist, 'vfXCStride', 2, ierr)
      call NhlFRLSetInteger (rlist, 'vfYCStride', 2, ierr)
      call NhlFRLSetFloat (rlist, 'vfMissingUValueV', -9999.0, ierr)
      call NhlFCreate (vfield, 'VectorField', NhlFvectorFieldClass, 
     +     appid, rlist, ierr)

      do 30 i = 1,lonlen
         do 20 j = 1,latlen
            if (P(i,j) .ne. -9999.0 ) then
               P (i,j) = P(i,j) / 100.0
            endif
            if( T(i,j) .ne. -9999.0 ) then
               T (i,j) = (T(i,j) - 273.15)*9.0/5.0 +32.0
            endif
 20      continue
 30   continue

      call NhlFRLClear (rlist)
      call NhlFRLSetMDFloatArray (rlist, 'sfDataArray', P, 2,
     +                            len_dims, ierr)
      call NhlFRLSetFloat (rlist, 'sfXCStartV', lon(1), ierr)
      call NhlFRLSetFloat (rlist, 'sfYCStartV', lat(1), ierr)
      call NhlFRLSetFloat (rlist, 'sfXCEndV', lon(lonlen), ierr)
      call NhlFRLSetFloat (rlist, 'sfYCEndV', lat(latlen), ierr)
      call NhlFRLSetInteger (rlist, 'sfXCStride', 2, ierr)
      call NhlFRLSetInteger (rlist, 'sfYCStride', 2, ierr)
      call NhlFRLSetFloat (rlist, 'sfMissingValueV', -9999.0, ierr)
      call NhlFCreate (sfield, 'ScalarField', NhlFscalarFieldClass,
     +     appid, rlist, ierr)

      call NhlFRLClear (rlist)
      call NhlFRLSetMDFloatArray (rlist, 'sfDataArray' , T, 2, 
     +     len_dims, ierr)
      call NhlFRLSetFloat (rlist, 'sfXCStartV', lon(1), ierr)
      call NhlFRLSetFloat (rlist, 'sfYCStartV', lat(1), ierr)
      call NhlFRLSetFloat (rlist, 'sfXCEndV', lon(lonlen), ierr)
      call NhlFRLSetFloat (rlist, 'sfYCEndV', lat(latlen), ierr)
      call NhlFRLSetInteger (rlist, 'sfXCStride', 2, ierr)
      call NhlFRLSetInteger (rlist, 'sfYCStride', 2, ierr)
      call NhlFRLSetFloat (rlist, 'sfMissingValueV', -9999.0, ierr)
      call NhlFCreate (sfield2, 'ScalarField2', NhlFscalarFieldClass,
     +     appid, rlist, ierr)

C
C  To zoom in on a certain area of the first plot adjust the following
C  four numbers.
C
C  The following four numbers will cause the plots to display the
C  entire United States.
C

      if (ZOOM .eq. 0) then
         MinLat = 18.0
         MaxLat = 65.0
         MinLon = -128.0
         MaxLon = -58.0

C
C  The Following four numbers will zoom in on the great lakes region of 
C  the United States.
C

      else if (ZOOM .eq. 1) then
         MinLat = 40.0
         MaxLat = 60.0
         MinLon = -100.0
         MaxLon = -58.0
      end if

C
C Create a map object
C

      call NhlFRLClear (rlist)
      call NhlFRLSetFloat (rlist, 'vpXF', 0.03, ierr)
      call NhlFRLSetFloat (rlist, 'vpYF', 0.85, ierr)
      call NhlFRLSetFloat (rlist, 'vpWidthF', 0.8, ierr)
      call NhlFRLSetFloat (rlist, 'vpHeightF', 0.8, ierr)
      call NhlFRLSetString (rlist, 'vpUseSegments', 'true', ierr)
      call NhlFRLSetFloat (rlist, 'mpMinLatF', MinLat, ierr)
      call NhlFRLSetFloat (rlist, 'mpMaxLatF', MaxLat, ierr)
      call NhlFRLSetFloat (rlist, 'mpMinLonF', MinLon, ierr)
      call NhlFRLSetFloat (rlist, 'mpMaxLonF', MaxLon, ierr)
      call NhlFRLSetFloat (rlist, 'mpCenterLonF', -100.0, ierr)
      call NhlFRLSetFloat (rlist, 'mpCenterLatF', 40.0, ierr)
      call NhlFRLSetString (rlist, 'mpGridAndLimbDrawOrder',
     +     'predraw', ierr)
      call NhlFCreate (mapid, 'map', NhlFmapPlotClass, wid, 
     +     rlist, ierr)

      call NhlFRLClear (rlist)
      call NhlFRLSetString (rlist, 'cnFillOn', 'true', ierr)
      call NhlFRLSetString (rlist, 'cnLinesOn', 'false', ierr)
      call NhlFRLSetString (rlist, 'cnFillDrawOrder', 'predraw',
     +     ierr)
      call NhlFRLSetInteger(rlist, 'cnScalarFieldData', sfield,
     +     ierr)
      call NhlFRLSetString (rlist, 'pmLabelBarDisplayMode',
     +     'always', ierr)
      call NhlFRLSetFloat  (rlist, 'pmLabelBarHeightF', 0.075,
     +     ierr)
      call NhlFRLSetFloat  (rlist, 'pmLabelBarWidthF', 0.6, ierr)
      call NhlFRLSetString (rlist, 'lbOrientation', 'horizontal',
     +     ierr)
      call NhlFRLSetString (rlist, 'lbPerimOn', 'False', ierr)
      call NhlFRLSetString (rlist, 'pmLabelBarSide', 'top', ierr)
      call NhlFCreate (cnid, 'contourplot', NhlFcontourPlotClass,
     +     wid, rlist, ierr)
      
C
C Create a VectorPlot object using the above data field.
C

      call NhlFRLClear (rlist)
      call NhlFRLSetString  (rlist,'vcUseScalarArray','true',ierr)
      call NhlFRLSetInteger (rlist,'vcVectorFieldData', vfield,
     +     ierr)
      call NhlFRLSetInteger (rlist,'vcScalarFieldData', sfield2,
     +     ierr)
      call NhlFRLSetFloat   (rlist,'vcMinFracLengthF', 0.33, ierr)
      call NhlFRLSetString  (rlist,'vcMonoLineArrowColor', 'false', 
     +     ierr)
      call NhlFRLSetString  (rlist,'vcVectorDrawOrder', 'predraw',
     +     ierr)
      call NhlFRLSetString  (rlist,'pmLabelBarDisplayMode', 
     +     'always', ierr)
      call NhlFRLSetFloat  (rlist, 'pmLabelBarWidthF', 0.1, ierr)
      call NhlFRLSetString (rlist, 'lbPerimOn', 'False', ierr)
      call NhlFCreate (vcid, 'vectorplot', NhlFvectorPlotClass,
     +     wid, rlist, ierr)

      write(day,34) timestep(1)
 34   format(I3)

      mainstring = reftime // '05  00:00' // day

      call NhlFRLClear (rlist)
      call NhlFRLSetFloat  (rlist, 'vpXF', 0.03, ierr)
      call NhlFRLSetFloat  (rlist, 'vpYF', 0.85, ierr)
      call NhlFRLSetFloat  (rlist, 'vpWidthF', 0.8, ierr)
      call NhlFRLSetFloat  (rlist, 'vpHeightF', 0.8, ierr)
      call NhlFRLSetString (rlist, 'tiMainFuncCode', "~", ierr)
      call NhlFRLSetInteger(rlist, 'tiMainFont', 25, ierr)
      call NhlFRLSetString (rlist, 'tiMainString', mainstring, 
     +     ierr)
      call NhlFCreate (tiid1, 'Titles', NhlFtitleClass, wid, 
     +     rlist, ierr)

      call NhlFRLClear (rlist)
      call NhlFRLSetFloat  (rlist, 'vpXF', 0.03, ierr)
      call NhlFRLSetFloat  (rlist, 'vpYF', 0.9, ierr)
      call NhlFRLSetFloat  (rlist, 'vpWidthF', 0.8, ierr)
      call NhlFRLSetFloat  (rlist, 'vpHeightF', 0.8, ierr)
      call NhlFRLSetInteger(rlist, 'tiMainFont', 25, ierr)
      call NhlFRLSetString (rlist, 'tiMainString',
     +     'January 1996 Snow Storm',ierr)
      call NhlFCreate (tiid2, 'Titles', NhlFtitleClass, wid, 
     +     rlist, ierr)

      call NhlFRLClear (rlist)
      call NhlFRLSetFloat  (rlist, 'txPosXF', 0.25,ierr)
      call NhlFRLSetFloat  (rlist, 'txPosYF', 0.08,ierr)
      call NhlFRLSetFloat (rlist, 'txFontHeightF', 0.015,ierr)
      call NhlFRLSetString (rlist, 'txString','Contours represent pressu
     +re field.:C:Vectors represent wind direction:C:colored by temperat
     +ure.',ierr)
      call NhlFCreate (txid1, 'text', NhlFtextItemClass, wid, rlist,
     +     ierr)

      call NhlFAddOverlay (mapid, cnid, -1, ierr)
      call NhlFAddOverlay (mapid, vcid, -1, ierr)

      do 100 i = (2*(timlen - 1)/3)+1,timlen

        tm = timestep(i)
        if (tm .ne. 102.0 .and. tm .ne. 222.0 .and. tm .ne. 216.0) then

           call get_2d_array (U, latlen, lonlen, uf, u_id, i)
           call get_2d_array (V, latlen, lonlen, vf, v_id, i)
           call get_2d_array (P, latlen, lonlen, pf, p_id, i)
           call get_2d_array (T, latlen, lonlen, tf, t_id, i)

           do 50 j = 1,lonlen
              do 40 k = 1,latlen
                 if (P(j,k) .ne. -9999.0 ) then
                    P (j,k) = P(j,k) / 100.0
                 endif
                 if (T(j,k) .ne. -9999.0 ) then
                    T (j,k) = (T(j,k) - 273.15)*9.0/5.0 +32.0
                 endif
 40           continue
 50        continue

           write(day,54) timestep(i)
 54        format(I3)

           mainstring = reftime // '05 00:00 + ' // day
          
           call NhlFRLClear (rlist)
           call NhlFRLSetMDFloatArray (rlist, 'vfUDataArray', U, 2,
     +          len_dims, ierr)
           call NhlFRLSetMDFloatArray (rlist, 'vfVDataArray', V, 2,
     +          len_dims, ierr)
           call NhlFSetValues (vfield, rlist, ierr)

           call NhlFRLClear (rlist)
           call NhlFRLSetMDFloatArray (rlist, 'sfDataArray', P, 2,
     +          len_dims, ierr)
           call NhlFSetValues (sfield, rlist, ierr)

           call NhlFRLClear (rlist)
           call NhlFRLSetMDFloatArray (rlist, 'sfDataArray', T, 2,
     +          len_dims, ierr)
           call NhlFSetValues (sfield2, rlist, ierr)
           
           call NhlFRLClear (rlist)
           call NhlFRLSetString (rlist, 'tiMainString', mainstring,
     +          ierr)
           call NhlFSetValues (tiid1, rlist, ierr)
    
           call NhlFDraw(mapid,ierr)
           call NhlFDraw(tiid1, ierr)
           call NhlFDraw(tiid2, ierr)
           call NhlFDraw(txid1, ierr)
           call NhlFFrame(wid, ierr)

        endif
 100  continue

C
C Destroy the workstation object and exit.
C

      call NhlFDestroy (wid,ierr)
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
