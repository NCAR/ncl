C
C      $Id: xy10f.f,v 1.11 2010-03-15 22:49:25 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       xy10f.f
C
C  Author:     Mary Haley
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Fri May  5 11:18:07 MDT 1995
C
C Description:        This example shows how to overlay an XyPlot
C                     on a MapPlot.
C
      external NhlFAppClass 
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFXyPlotClass
      external NhlFMapPlotClass
      external NhlFCoordArraysClass
C
C Define the day and the hour for which we are getting data values.
C (March 18, 1995, hour 0).
C
      character*21 file
      data file/'/cdf/95031800_sao.cdf'/
C
C Declare variables for the HLU routine calls.
C
      integer appid, xworkid, xyid1, xyid2, mpid1, mpid2, dataid
      integer rlist, i
      real special_value
      real lat(3000), lon(3000)
      data special_value/-9999./
C
C Declare variables for getting information from netCDF file.
C
      character*50 recname
      character*256 filename
      integer ncid, latid, lonid, recid
      integer ndims, nvars, ngatts, rec_len
      integer start(1), count(1)

      CHARACTER*7  wks_type
C
C Define the workstation type
C
      wks_type = "x11"
C
C
C Initialize the HLU library and set up resource template.
C
      call NhlFInitialize
      call NhlFRLCreate(rlist,'setrl')
C
C Create Application object.  The Application object name is used to
C determine the name of the resource file, which is "xy10.res" in
C this case.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'xy10',NhlFAppClass,0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName','./xy10f.ncgm',ierr)
         call NhlFCreate(xworkid,'xy10Work',
     +        NhlFNcgmWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPause','True',ierr)
         call NhlFCreate(xworkid,'xy10Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,rlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PS workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPSFileName','./xy10f.ps',ierr)
         call NhlFCreate(xworkid,'xy10Work',
     +        NhlFPSWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPDFFileName','./xy10f.pdf',ierr)
         call NhlFCreate(xworkid,'xy10Work',
     +        NhlFPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName','./xy10f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(xworkid,'xy10Work',
     +        NhlFcairoPSPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName','./xy10f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(xworkid,'xy10Work',
     +        NhlFcairoImageWorkstationClass,0,rlist,ierr)
      endif
C
C Open the netCDF file.
C
      call gngpat(filename,"data",ierr)
      do 32 i=1,256
         if( filename(i:i).eq.char(0) ) then
            filename(i:i+20)=file
            goto 34
         endif
 32   continue
C      
C The second argument to 'ncopn' should be NCNOWRIT, but since we
C can't include 'netcdf.inc', we are using the value '0' instead.
C
 34   ncid = ncopn(filename,0,ierr)
C
C Get the record length and dimension name.
C
      call ncinq(ncid, ndims, nvars, ngatts, recid, ierr)
      call ncdinq(ncid, recid, recname, rec_len, ierr)
C
C Get the ids of the lat/lon arrays.
C
      latid = ncvid(ncid,'lat',ierr)
      lonid = ncvid(ncid,'lon',ierr)
C
C Get lat/lon data values.
C
      start(1) = 1
      count(1) = rec_len
      call ncvgt(ncid,latid,start,count,lat,ierr)
      call ncvgt(ncid,lonid,start,count,lon,ierr)
C
C Close the netCDF file.
C
      call ncclos(ncid,ierr)
C
C Throw out values that may be incorrect.
C
      do 50 i=1,rec_len
         if( lat(i) .lt. -90. .or. lat(i) .gt. 90.) then
            lat(i) = special_value
         endif
         if( lon(i) .lt. -180. .or. lon(i) .gt. 180.) then
            lon(i) = special_value
         endif
 50   continue
C
C Define the Data object.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloatArray(rlist,'caXArray',lon,rec_len,ierr)
      call NhlFRLSetFloatArray(rlist,'caYArray',lat,rec_len,ierr)
      call NhlFCreate(dataid,'xyData',NhlFCoordArraysClass,0,
     +     rlist,ierr)
C
C The id for this Data object is now the resource value for
C xyCoordData.  Tweak some XyPlot resources in the resource file
C ("xy10.res").
C
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'xyCoordData',dataid,ierr)
      call NhlFCreate(xyid1,'xyPlot1',NhlFXyPlotClass,xworkid,
     +     rlist,ierr)
C     
C Plot all of the station ids.
C
      call NhlFDraw(xyid1,ierr)
      call NhlFFrame(xworkid,ierr)
C
C Create a second XyPlot object with a different name so we can
C change the "tr" resources to limit the plot within the mainland
C United States.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'xyCoordData',dataid,ierr)
      call NhlFCreate(xyid2,'xyPlot2',NhlFXyPlotClass,xworkid,
     +     rlist,ierr)
C
C Plot station ids over mainland United States only.  This is done
C using resources in the "xy10.res" resource file.
C
      call NhlFDraw(xyid2,ierr)
      call NhlFFrame(xworkid,ierr)
C
C Create two MapPlots, one of the United States  and one of just
C Colorado (projection parameters are set up in the "xy10.res"
C resource file).
C
      call NhlFCreate(mpid1,'mpPlot1',NhlFMapPlotClass,xworkid,
     +     0,ierr)

      call NhlFCreate(mpid2,'mpPlot2',NhlFMapPlotClass,xworkid,
     +     0,ierr)
C
C Draw the two plots.
C
      call NhlFDraw(mpid1,ierr)
      call NhlFFrame(xworkid,ierr)

      call NhlFDraw(mpid2,ierr)
      call NhlFFrame(xworkid,ierr)
C
C Overlay the first XyPlot object on the first MapPlot object.  This
C will plot station ids over the mainland United States.
C
      call NhlFAddOverlay(mpid1,xyid1,-1,ierr)
      call NhlFDraw(mpid1,ierr)
      call NhlFFrame(xworkid,ierr)
C
C Overlay the second XyPlot object on the second MapPlot object.  This
C will plot station ids over Colorado.
C
      call NhlFAddOverlay(mpid2,xyid2,-1,ierr)
      call NhlFDraw(mpid2,ierr)
      call NhlFFrame(xworkid,ierr)
C
C NhlFDestroy destroys the given id and all of its children so
C destroying "appid will destroy "xworkid" which will also destroy
C "plotid".
C
      call NhlFRLDestroy(rlist)
      call NhlFDestroy(xworkid,ierr)
C
C Restores state.
C
      call NhlFClose
      stop
      end

