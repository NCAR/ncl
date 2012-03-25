C
C  $Id: vc06f.f,v 1.8 2010-03-15 22:49:25 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C               Copyright (C)  1996                                    C
C       University Corporation for Atmospheric Research                C
C               All Rights Reserved                                    C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       vc06f.f
C
C  Author:     David Brown (converted by Lynn Hermanson)
C              National Center for Atmospheric Research
C              PO 3000, Boulder, Colorado
C
C  Date:       June 24, 1996
C
C    Description:  This example demonstrates several features of
C                 VectorPlot:
C                 1) Use of the vcMinDistanceF resource to reduce
C                    the crowding of vector arrows in regions where
C                    the transformation compresses the distance
C                    between adjacent grid points.
C                 2) Use of a scalarfield to determine the color
C                    of the vector arrow fill.
C                 3) VectorPlot as an overlay of MapPlot.
C                 Successive frames show the result of increasing the
C                 vcMinDistanceF in small steps. At the same time the
C                 MapTransformation mpCenterLonF resource is decreased in
C                 steps, causing the orthographic projection of the
C                 northern hemisphere to appear to rotate when the
C                 output is animated. Increasing the value of the
C                 FRAME_COUNT variable will result in a smoother
C                 animation.
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
      external NhlFScalarFieldClass
      external NhlFMapPlotClass
      
      character*7  wks_type
      integer appid,wid,vcid,vfid, sfid, mpid
      integer rlist
      real U(73,73),V(73,73), PSL(73,73)
      character*5  smindist0 
      character*4  smindist1
      character*3  smindist2
      character*7  degree
      character*26  title0
      character*34  title
      character*8  smindist
      character*4  mindist
      character*3  longval
      character*10 slongitude
      character*50 recname
C
C Declare variables for getting information from netCDF file.
C
      integer uv, p, u_id, v_id, p_id, lon_id, lat_id, FRAME_COUNT
      integer  i, mindistval, icount(3), longitudeval
      real val
      integer  start(2), count(2), lonlen, latlen, flen
      real CenLonF
      character*256  filenameUV
      character*256  filenamePsl

C
C Open the netCDF file.
C
      call gngpat(filenameUV,'data',ierr)
      flen = 17
      do 10 i=1,256
         if( filenameUV(i:i).eq.char(0) ) then
            filenameUV(i:i+flen)='/cdf/941110_UV.cdf'
            goto 15
         endif
 10   continue
 15   uv = ncopn(filenameUV,0,ierr)
C
C
C Open the netCDF file.
C
      call gngpat(filenamePsl,'data',ierr)
      flen = 16
      do 11 i=1,256
         if( filenamePsl(i:i).eq.char(0) ) then
            filenamePsl(i:i+flen)='/cdf/941110_P.cdf'
            goto 16
         endif
 11   continue
C
 16   p = ncopn(filenamePsl,0,ierr)
C
C Generate vector data array
C
      FRAME_COUNT=13
C
C Initialize the high level utility library
C
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
      call NhlFCreate(appid,'vc06',NhlFAppClass,0,rlist,ierr)
C
C Default is to create an X11 window.
C
      wks_type = "x11"

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkMetaName','./vc06f.ncgm',ierr)
      call NhlFCreate(wid,'vc06Work',

     +        NhlFNcgmWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkPause','True',ierr)
      call NhlFCreate(wid,'vc06Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,rlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkPSFileName','./vc06f.ps',ierr)
      call NhlFCreate(wid,'vc06Work',
     +        NhlFPSWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkPDFFileName','./vc06f.pdf',ierr)
      call NhlFCreate(wid,'vc06Work',
     +        NhlFPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkFileName','./vc06f',ierr)
      call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
      call NhlFCreate(wid,'vc06Work',
     +        NhlFCairoPSPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkFileName','./vc06f',ierr)
      call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
      call NhlFCreate(wid,'vc06Work',
     +        NhlFCairoImageWorkstationClass,0,rlist,ierr)
      endif
C
C Create a VectorField data object using the data set defined above.
C By default the array bounds will define the data boundaries 
C   (zero-based,as in C language conventions)
C
C
C Get the U and V and lat/lon dimensions.
C
      lat_id = ncdid(uv,'lat',ierr)
      lon_id = ncdid(uv,'lon',ierr)
      u_id = ncvid(uv,'u',ierr)
      v_id = ncvid(uv,'v',ierr)
      call ncdinq(uv,lat_id,recname,latlen,ierr)
      call ncdinq(uv,lon_id,recname,lonlen,ierr)

      start(1) = 1
      start(2) = 1
      count(1) = latlen
      count(2) = lonlen
      call ncvgt(uv,u_id,start,count,U,ierr)
      call ncvgt(uv,v_id,start,count,V,ierr)

      icount(1) = latlen
      icount(2) = lonlen

      call NhlFRLClear(rlist)
      call NhlFRLSetMDFloatArray(rlist,'vfUDataArray',U,2,
     +icount,ierr)
      call NhlFRLSetMDFloatArray(rlist,'vfVDataArray',V,2,
     +icount,ierr)
      call NhlFRLSetFloat(rlist,'vfXCStartV', -180.0,ierr)
      call NhlFRLSetFloat(rlist,'vfXCEndV', 180.0,ierr)
      call NhlFRLSetFloat(rlist,'vfYCStartV',-90.0,ierr)
      call NhlFRLSetFloat(rlist,'vfYCEndV', 90.0,ierr)
      call NhlFCreate(vfid,'vectorfield',NhlFvectorFieldClass,appid,
     +rlist,ierr)
C
C Create a ScalarField data object using the data set defined above.
C By default the array bounds will define the data boundaries
C (zero-based, as in C language conventions)
C
C Get the PSL and lat/lon dimensions.
C
      lat_id = ncdid(p,'lat',ierr)
      lon_id = ncdid(p,'lon',ierr)
      p_id = ncvid(p,'Psl',ierr)
      call ncdinq(p,lat_id,recname,latlen,ierr)
      call ncdinq(p,lon_id,recname,lonlen,ierr)

      start(1) = 1
      start(2) = 1
      count(1) = latlen
      count(2) = lonlen
      call ncvgt(p,p_id,start,count,PSL,ierr)

      icount(1) = latlen
      icount(2) = lonlen

      call NhlFRLClear(rlist)
      call NhlFRLSetMDFloatArray(rlist,'sfDataArray',PSL,2,
     +icount,ierr)
      call NhlFRLSetFloat(rlist,'sfXCStartV', -180.0,ierr)
      call NhlFRLSetFloat(rlist,'sfXCEndV', 180.0,ierr)
      call NhlFRLSetFloat(rlist,'sfYCStartV', -90.0,ierr)
      call NhlFRLSetFloat(rlist,'sfYCEndV', 90.0,ierr)
      call NhlFCreate(sfid,'scalarfield',NhlFscalarFieldClass,appid,
     +rlist,ierr)
C
C Create a VectorPlot object, supplying the VectorField object as data
C Setting vcMonoFillArrowFillColor False causes VectorPlot to color
C the vector arrows individually based, by default, on the vector
C magnitude. Also supply the ScalarField object that will be used
C to determine the color of each individual vector arrow.
C Setting vcMonoVectorLineColor False causes VectorPlot to color the
C vector arrows individually and setting vcUseScalarArray True results
C in VectorPlot applying the colors based on the contents of the
C scalarfield.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vcRefMagnitudeF', 20.0,ierr)
      call NhlFRLSetString(rlist,'vcUseScalarArray', 'True',ierr)
      call NhlFRLSetString(rlist,'vcFillArrowsOn', 'True',ierr)
      call NhlFRLSetString(rlist,'vcMonoFillArrowFillColor', 'False'
     +,ierr)
      call NhlFRLSetFloat(rlist,'vcMinFracLengthF', 0.25,ierr)
      call NhlFRLSetInteger(rlist,'vcVectorFieldData',vfid,ierr)
      call NhlFRLSetInteger(rlist,'vcScalarFieldData',sfid,ierr)
      call NhlFCreate(vcid,'vectorplot',NhlFvectorPlotClass,wid,rlist,
     +ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'mpProjection', 'ORTHOGRAPHIC',ierr)
      call NhlFRLSetFloat(rlist,'mpCenterLatF', 50.0,ierr)
      call NhlFCreate(mpid,'mapplot',NhlFmapPlotClass,wid,rlist,ierr)

      call NhlFAddOverlay(mpid,vcid, -1,ierr)
C
C Strings used to create fixed length numbers
C
      smindist0 = '0.000'
      smindist1 = '0.00'
      smindist2 = '0.0'
      degree = ':S:o:N:'
C
C Create FRAME_COUNT frames, increasing the value of vcMinDistanceF
C and decreasing the value of mpCenterLonF at each successive frame.
C
C Note that the first frame and the last frame are equivalent in
C longitude.
C
      do 1000 i = (FRAME_COUNT-1),0,-1
         call NhlFRLClear(rlist)
         CenLonF =  i * 360./(FRAME_COUNT-1)
         call NhlFRLSetFloat(rlist,'mpCenterLonF',CenLonF,ierr)
         call NhlFSetValues(mpid,rlist,ierr)
C
C create fixed length strings representing the current longitude
C and the value of vcMinDistanceF
C
         longitudeval = (i * 360./(FRAME_COUNT-1) + 0.5)
         write(longval,50)longitudeval
 50      format(I3)
         slongitude = longval//degree    

         val = ((FRAME_COUNT-1) - i) * 0.0175/(FRAME_COUNT-1)
         mindistval = (10000*val + 0.5)

         if (mindistval .lt. 10) then
            write(mindist,100)mindistval
 100        format(I1)
            smindist = smindist0//mindist
         else
            if (mindistval .lt. 100) then
               write(mindist,101)mindistval
 101           format(I2) 
               smindist = smindist1//mindist
            else
               write(mindist,102)mindistval
 102           format(I3)
               smindist = smindist2//mindist      
            endif
         endif
         
         call NhlFRLClear(rlist)

         title0 = 'Varying vcMinDistanceF :: '
         title = title0//smindist
         call NhlFRLSetString(rlist,'tiMainString',title,ierr)
         call NhlFRLSetString(rlist,'tiXAxisString',slongitude,
     +        ierr)
         call NhlFRLSetFloat(rlist,'vcMinDistanceF',val,ierr)
         call NhlFSetValues(vcid,rlist,ierr)

         call NhlFDraw(mpid,ierr)
         call NhlFFrame(wid,ierr)
 1000 continue 
C
C Destroy the objects created, close the HLU library and exit.
C
      call NhlFDestroy(mpid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFDestroy(appid,ierr)
      call NhlFClose
      stop
      end
