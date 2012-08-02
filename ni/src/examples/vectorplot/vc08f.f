C
C  $Id: vc08f.f,v 1.4 2010-03-15 22:49:25 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C               Copyright (C)  1996                                    C
C       University Corporation for Atmospheric Research                C
C               All Rights Reserved                                    C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       vc08f.f
C
C   Author:     Bob Lackman
C               National Center for Atmospheric Research
C               PO 3000, Boulder, Colorado
C
C   Date:       Wed Jan 24, 1996
C
C   Description:    Plots wind vectors at a grid stride of 3.
C                   Vectors are colored by wind speed.
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFVectorPlotClass
      external NhlFVectorFieldClass
      external NhlFMapPlotClass
      external NhlFTextItemClass
      parameter(NLON = 129, NLAT = 64)
      
      character*7  wks_type
      integer appid,wid,vcid,vfield,mapid,txid1,txid2
      integer rlist
      real U(NLON,NLAT),V(NLON,NLAT)
      integer vccolors(14)
      data vccolors/26,28,30,33,36,39,42,45,48,51,54,56,58,60/
C
C Declare variables for getting information from netCDF file.
C
      integer vf, u_id, v_id, lon_id, lat_id
      integer  i, start(4), count(4), lonlen, latlen, flen
      real lon(NLON), lat(NLAT)
      character*256  filename
      character*50 recname
      character*256 title, subtitle, txtstring
C
C Open the netCDF file.
C
      call gngpat(filename,'data',ierr)
      flen = 22
      do 10 i=1,256
         if( filename(i:i).eq.char(0) ) then
            filename(i:i+flen)='/cdf/ex01B1_uv300.hs.nc'
            goto 15
         endif
 10   continue
 15   vf = ncopn(filename,0,ierr)
C
C Initialize the high level utility library
C
C Create an application context. Set the app dir to the current
C directory so the application looks for a resource file in the working
C directory. 
C
      call NhlFInitialize
      call NhlFRLCreate(rlist,'setrl')
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'vc08',NhlFAppClass,0,rlist,ierr)
C
C Default is to create an X11 window.
C
      wks_type = "x11"

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkColorMap','uniform',ierr)
      call NhlFRLSetString(rlist,'wkMetaName','./vc08f.ncgm',ierr)
      call NhlFCreate(wid,'vc08Work',
     +        NhlFNcgmWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkColorMap','uniform',ierr)
      call NhlFRLSetString(rlist,'wkPause','True',ierr)
      call NhlFCreate(wid,'vc08Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,rlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkColorMap','uniform',ierr)
      call NhlFRLSetString(rlist,'wkPSFileName','./vc08f.ps',ierr)
      call NhlFCreate(wid,'vc08Work',
     +        NhlFPSWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkColorMap','uniform',ierr)
      call NhlFRLSetString(rlist,'wkPDFFileName','./vc08f.pdf',ierr)
      call NhlFCreate(wid,'vc08Work',
     +        NhlFPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkColorMap','uniform',ierr)
      call NhlFRLSetString(rlist,'wkFileName','./vc08f',ierr)
      call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
      call NhlFCreate(wid,'vc08Work',
     +        NhlFCairoPSPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkColorMap','uniform',ierr)
      call NhlFRLSetString(rlist,'wkFileName','./vc08f',ierr)
      call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
      call NhlFCreate(wid,'vc08Work',
     +        NhlFCairoImageWorkstationClass,0,rlist,ierr)
      endif
C
C Get netCDF file information.
C
      lat_id = ncdid(vf,'latitude',ierr)
      lon_id = ncdid(vf,'longitude',ierr)
      u_id = ncvid(vf,'U',ierr)
      v_id = ncvid(vf,'V',ierr)
      call ncdinq(vf,lat_id,recname,latlen,ierr)
      call ncdinq(vf,lon_id,recname,lonlen,ierr)
C
C Get longitude and latitude values.
C
      start(1) = 1
      lat_id = ncvid(vf,'lat',ierr)
      count(1) = latlen
      call ncvgt(vf,lat_id,start,count,lat,ierr)

      start(1) = 1
      lon_id = ncvid(vf,'lon',ierr)
      count(1) = lonlen
      call ncvgt(vf,lon_id,start,count,lon,ierr)
C
C Get U and V data values.
C
      start(1) = 1
      start(2) = 1
      start(3) = 1
      start(4) = 1
      count(1) = lonlen
      count(2) = latlen
      count(3) = 1
      count(4) = 1
      call ncvgt(vf,u_id,start,count,U,ierr)
      call ncvgt(vf,v_id,start,count,V,ierr)
C
C Get the titles (global attributes).
C
      call ncagtc(vf,0,'title',title,256,ierr)
      call ncagtc(vf,0,'sub_title_rhs',subtitle,256,ierr)
C
C Close the netCDF file.
C
      call ncclos(vf,ierr)
C
C Create a VectorField data object, U and V are 4-D arrays which
C are a function of time, level, latitude and longitude.  Use the
C 1st time & level.
C
      count(1) = lonlen
      count(2) = latlen

      call NhlFRLClear(rlist)
      call NhlFRLSetMDFloatArray(rlist,'vfUDataArray',U,2,
     +     count,ierr)
      call NhlFRLSetMDFloatArray(rlist,'vfVDataArray',V,2,
     +     count,ierr)
      call NhlFRLSetFloat(rlist,'vfXCStartV',lon(1),ierr)
      call NhlFRLSetFloat(rlist,'vfXCEndV', lon(lonlen),ierr)
      call NhlFRLSetFloat(rlist,'vfYCStartV',lat(1),ierr)
      call NhlFRLSetFloat(rlist,'vfYCEndV',lat(latlen),ierr)
C
C Specify a stride of 3 in both dimensions
C
      call NhlFRLSetInteger(rlist,'vfXCStride',3,ierr)
      call NhlFRLSetInteger(rlist,'vfYCStride',3,ierr)
      call NhlFCreate(vfield,'VectorField',NhlFvectorFieldClass,appid,
     +     rlist,ierr)
C
C Create a VectorPlot object "vcid" and connect the data object "vfield".
C
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'vcVectorFieldData',vfield,ierr)
      call NhlFRLSetFloat(rlist,'vcMinFracLengthF',0.33,ierr)
      call NhlFRLSetFloat(rlist,'vcRefLengthF',0.025,ierr)
      call NhlFRLSetString(rlist,'vcRefAnnoString1','$VMG$ '//subtitle,
     +     ierr)
      call NhlFRLSetString(rlist,'vcMonoLineArrowColor','false',ierr)
      call NhlFRLSetString(rlist,'pmLabelBarDisplayMode','always',ierr)
      call NhlFRLSetString(rlist,'pmLabelBarSide','bottom',ierr)
      call NhlFRLSetString(rlist,'lbOrientation','horizontal' ,ierr)
      call NhlFRLSetString(rlist,'lbTitleString',subtitle,ierr)
      call NhlFRLSetIntegerArray(rlist,'vcLevelColors',vccolors,14,ierr)
C     call NhlFRLSetFloat(rlist, 'vcLineArrowThicknessF',1.75,ierr)
      call NhlFCreate(vcid,'vectorplot',NhlFvectorPlotClass,wid,rlist,
     +ierr)
C
C Create a map object.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vpXF',0.05,ierr)
      call NhlFRLSetFloat(rlist,'vpWidthF',0.9,ierr)
      call NhlFRLSetFloat(rlist,'vpYF',0.85,ierr)
      call NhlFRLSetString(rlist,'mpGridAndLimbDrawOrder','predraw',
     +     ierr)
      call NhlFCreate(mapid,'map',NhlFmapPlotClass,wid,rlist,ierr)

      call NhlFAddOverlay(mapid,vcid, -1,ierr)
      call NhlFDraw(mapid,ierr)
C
C Create a text item object as a main title.
C         
      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'txPosXF',0.5,ierr)
      call NhlFRLSetFloat(rlist,'txPosYF',0.85,ierr)
      call NhlFRLSetString(rlist,'txJust','CENTERCENTER',ierr)
      call NhlFRLSetString(rlist,'txString',title,ierr)
      call NhlFRLSetFloat(rlist,'txFontHeightF',.030,ierr)
      call NhlFRLSetInteger(rlist,'txFont',25,ierr)
      call NhlFCreate(txid1,'main',NhlFtextItemClass,wid,rlist,ierr)
C
C Create a subheader text item object.
C
      txtstring = 'Wind                                                 
     +         300mb                                                    
     +   (m/s)'
      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'txPosXF',0.5,ierr)
      call NhlFRLSetFloat(rlist,'txPosYF',0.80,ierr)
      call NhlFRLSetString(rlist,'txJust','CENTERCENTER',ierr)
      call NhlFRLSetString(rlist,'txString',txtstring,ierr)
      call NhlFRLSetFloat(rlist,'txFontHeightF',.015,ierr)
      call NhlFRLSetINteger(rlist,'txFont',25,ierr)
      call NhlFCreate(txid2,'text',NhlFtextItemClass,wid,rlist,ierr)
      call NhlFDraw(txid1,ierr)
      call NhlFDraw(txid2,ierr)
      call NhlFFrame(wid,ierr)
C
C Destroy the objects created, close the HLU library and exit.
C
      call NhlFDestroy(wid,ierr)
      call NhlFDestroy(appid,ierr)
      call NhlFClose
      stop
      end

