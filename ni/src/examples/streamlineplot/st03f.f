C
C      $Id: st03f.f,v 1.3 1997-02-05 15:28:10 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1996                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       st03f.f
C
C  Author:     David Brown (converted by Mary Haley)
C              National Center for Atmospheric Research
C              PO 3000, Boulder, Colorado
C
C  Date:       Thu June 27 9:11:01 MST 1996
C
C  Description: 
C               This plot shows a StreamlinePlot overlaid on a polar
C               stereographic map projection. It illustrates some of
C               the problems with streamlines when the transformation to
C               NDC results in grid cells that vary widely in size.
C               Adjustment of certain parameters may improve the
C               appearance somewhat, but not as much as might be
C               desired. For this and other reasons, StreamlinePlot is
C               still undergoing development and its output may be
C               expected to change in the next release.
C               The data is extracted from an NMC forecast dataset for 
C               11/10/1994.
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFXWorkstationClass
      external NhlFVectorFieldClass
      external NhlFStreamlinePlotClass
      external NhlFMapPlotClass

      parameter(M=73,N=73)

      integer NCGM, X11, PS
      integer appid, wid, dataid, stid, mpid
      integer ncid, uid, vid, latid, lonid
      integer rlist, grlist
      integer len_dims(2)
      real U(M,N),V(M,N)
      real stepsize,spacing
      integer start(2), count(2), lonlen, latlen
      character*256 filename
      character*50 recname
      integer flen

      NCGM=0
      X11=1
      PS=0
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
      call NhlFRLCreate(grlist,'getrl')

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'st03',NhlFappClass,0,rlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName','./st03f.ncgm',ierr)
         call NhlFCreate(wid,'st03Work',
     +        NhlFNcgmWorkstationClass,0,rlist,ierr)
      else if (X11.eq.1) then
C
C Create an xworkstation object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'st03Work',NhlFXWorkstationClass,
     +        0,rlist,ierr)
      else if (PS.eq.1) then
C
C Create a PostScript workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPSFileName','./st03f.ps',ierr)
         call NhlFCreate(wid,'st03Work',
     +        NhlFPSWorkstationClass,0,rlist,ierr)
      endif
C
C Open the netCDF file.
C
      call gngpat(filename,'data',ierr)
      flen = 18
      do 10 i=1,256
         if( filename(i:i).eq.char(0) ) then
            filename(i:i+flen)='/cdf/941110_UV.cdf'
            goto 15
         endif
 10   continue
C      
C The second argument to 'ncopn' should be NCNOWRIT, but since we
C can't include 'netcdf.inc', we are using the value '0' instead.
C
 15   ncid = ncopn(filename,0,ierr)
C
C Get the data.
C
      latid = ncdid(ncid,'lat',ierr)
      lonid = ncdid(ncid,'lon',ierr)
      uid = ncvid(ncid,'u',ierr)
      vid = ncvid(ncid,'v',ierr)
      call ncdinq(ncid,latid,recname,latlen,ierr)
      call ncdinq(ncid,lonid,recname,lonlen,ierr)
      start(1) = 1
      start(2) = 1
      count(1) = lonlen
      count(2) = latlen
      call ncvgt(ncid,uid,start,count,U,ierr)
      call ncvgt(ncid,vid,start,count,V,ierr)
C
C Create a VectorField data object using the data set defined above.
C By default the array bounds will define the data boundaries (zero-based,
C as in C language conventions)
C
      len_dims(1) = lonlen
      len_dims(2) = latlen
      call NhlFRLClear(rlist)
      call NhlFRLSetMDFloatArray(rlist,'vfUDataArray',U,2,len_dims,ierr)
      call NhlFRLSetMDFloatArray(rlist,'vfVDataArray',V,2,len_dims,ierr)
      call NhlFRLSetFloat(rlist,'vfXCStartV', -180.0,ierr)
      call NhlFRLSetFloat(rlist,'vfXCEndV', 180.0,ierr)
      call NhlFRLSetFloat(rlist,'vfYCStartV', -90.0,ierr)
      call NhlFRLSetFloat(rlist,'vfYCEndV', 90.0,ierr)
      call NhlFRLSetFloat(rlist,'vfYCStartSubsetV', 0.0,ierr)
      call NhlFRLSetFloat(rlist,'vfYCEndSubsetV', 87.5,ierr)
      call NhlFCreate(dataid,'vfield',NhlFvectorFieldClass,appid,rlist,
     +     ierr)
C
C Create a StreamlinePlot object, supplying the VectorField object as data
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     +     'StreamlinePlot Overlaying MapPlot',ierr)
      call NhlFRLSetInteger(rlist,'stVectorFieldData',dataid,ierr)
      call NhlFCreate(stid,'streamlineplot',NhlFstreamlinePlotClass,wid,
     +     rlist,ierr)

      call NhlFCreate(mpid,'mapplot',NhlFmapPlotClass,wid,0,ierr)

      call NhlFAddOverlay(mpid,stid,-1,ierr)
      call NhlFDraw(mpid,ierr)
      call NhlFFrame(wid,ierr)
      
      call NhlFRLClear(grlist)
      call NhlFRLGetFloat(grlist,'stStepSizeF',stepsize,ierr)
      call NhlFRLGetFloat(grlist,'stMinLineSpacingF',spacing,ierr)
      call NhlFGetValues(stid,grlist,ierr)
C 
C Set the minimum arrow spacing
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     +     'Setting the Minimum Arrow Spacing',ierr)
      call NhlFRLSetFloat(rlist,'stMinArrowSpacingF',0.025,ierr)
      call NhlFSetValues(stid,rlist,ierr)

      call NhlFDraw(mpid,ierr)
      call NhlFFrame(wid,ierr)
C 
C Set the minimum line spacing
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString','Smaller Line Spacing',
     +     ierr)
      call NhlFRLSetFloat(rlist,'stMinLineSpacingF',spacing * 0.5,ierr)
      call NhlFSetValues(stid,rlist,ierr)

      call NhlFDraw(mpid,ierr)
      call NhlFFrame(wid,ierr)
C 
C Set the step size
C
      call NhlFRLClear(rlist,ierr)
      call NhlFRLSetString(rlist,'tiMainString','Smaller Step Size',
     +     ierr)
      call NhlFRLSetFloat(rlist,'stStepSizeF',stepsize * 0.5,ierr)
      call NhlFSetValues(stid,rlist,ierr)

      call NhlFDraw(mpid,ierr)
      call NhlFFrame(wid,ierr)
C 
C Clean up
C
      call NhlFDestroy(appid,ierr)
      call NhlFClose
      stop
      end
