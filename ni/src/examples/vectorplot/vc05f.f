C
C;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
C;                                                                      
C;                Copyright (C)  1996                                   
C;        University Corporation for Atmospheric Research               
C;                All Rights Reserved                                   
C;                                                                      
C;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
C 
C    File:       vc05f.f
C 
C    Author: Lynn Hermanson
C            National Center for Atmospheric Research
C            PO 3000, Boulder, Colorado
C  
C    Date:       April 11, 1996
C 
C    Description: Given a simple mathematically generated data set,
C                 which looks like the intersection of two vector fields
C                 with opposite directions,this program produces three
C                 plot frames which demonstrate the use of VectorPlot;1st-
C                 default resources, 2nd- overlayed on a world map, 3rd-
C                 overlayed on an orthographic projection of the globe
C                 centered on the north pole covering 90 to 40 degrees
C                 north latitude.
C 
C   Begin by generating the data set called "increasingvectors".
C   The values in the array "U" are defined to be the magnitude of the 
C   x component of the vectors.  The values in the array "V" are
C   defined to be the magnitude of the y component of the vectors. The
C   location of the vectors corresponds to the indices of the arrays.
C   Each index pair will eventually correspond to a latitude,longitude
C   location on a map. 
C ;;********* see resource file vc05.res ******

      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFMapPlotClass
      external NhlFVectorFieldClass
      external NhlFVectorPlotClass

      integer appid,wid,mapid,dataid,vcid
      integer srlist, ierr

      parameter(M=25,N=25)
      real U(M,N),V(M,N)

      integer len_dims(2) 
      real x,y
      integer i,j
      character*7  wks_type

      wks_type = "x11"

      DO 100 i= 1,25
         DO 200 j= 1,25
          x =  2.5*(i + 1)/1.4142
          y =  2.5*(j + 1)/1.4142
          U(i,j) = x
          V(i,j) = y
200   CONTINUE
100   CONTINUE

C Initialize the high level utility library

       call NhlFInitialize

C  Next,
C  create an application context. Set the app dir to the current
C  directory so the application looks for a resource file (vc05.res)
C  in the working directory. The necessary 
C  resources will be set in this program as each object is created.

C  Setup to choose the type of output workstation for all 3 frames.
C  Choose to display output to an X11 workstation.
C  Change the output from the
C  default colorMap (random sequence of colors) to a sequence
C  of colors so that vectors of increasing magnitude will move from
C  to cooler to hotter colors.

      call NhlFRLCreate(srlist,'SETRL')
      call NhlFRLClear(srlist)
      call NhlFRLSetstring(srlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'vc05',NhlFAppClass,0,
     1    srlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then

C Create a meta file workstation.
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkMetaName','./vc05f.ncgm',ierr)
         call NhlFCreate(wid,'vc05Work',NhlFNcgmWorkstationClass,0,
     1        srlist,ierr)
 
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then

C Create an X workstation.

         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkPause','true',ierr)

C ;;********* see resource file vc05.res ******
C       call NhlFRLSetstring(srlist,'wkColorMap', 'temp1',ierr)
C 
         call NhlFCreate(wid,'vc05Work',
     +        NhlFCairoWindowWorkstationClass,
     1        0,srlist,ierr)
    
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
         
C Create an older-style PS workstation.

         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkPSFileName','./vc05f.ps',ierr)
         call NhlFCreate(wid,'vc05Work',NhlFPSWorkstationClass,
     1        0,srlist,ierr)

      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then

C Create an older-style PDF workstation.

         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkPDFFileName','./vc05f.pdf',ierr)
         call NhlFCreate(wid,'vc05Work',NhlFPDFWorkstationClass,
     1        0,srlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then

C Create a cairo PS/PDF  workstation.

         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetstring(srlist,'wkFileName','./vc05f',ierr)
         call NhlFCreate(wid,'vc05Work',
     1        NhlFCairoPSPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then

C Create a cairo PNG  workstation.

         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetstring(srlist,'wkFileName','./vc05f',ierr)
         call NhlFCreate(wid,'vc05Work',
     1        NhlFCairoImageWorkstationClass,0,srlist,ierr)
      endif


C  BEGIN CREATING 1st FRAME

C  Create the 1st VectorField data object using the data set
C  generated above.

      call NhlFRLClear(srlist)
      len_dims(1) = N
      len_dims(2) = M
      call NhlFRLSetmdfloatarray(srlist,'vfUDataArray',U,2,
     1    len_dims,ierr)
      call NhlFRLSetmdfloatarray(srlist,'vfVDataArray',V,2,
     1    len_dims,ierr)
      call NhlFCreate(dataid,'increasingvectors',NhlFVectorFieldClass,
     1    appid,srlist,ierr)

C  Create the 1st VectorPlot object, supplying the 1st VectorField
C  object as data.
C  Turn off black and white arrows and use the temp1 colors spread
C  between
C  the lowest and highest vector magnitudes. The default vectors
C  appear as
C  solid lines with arrowheads in a linear coordinate system with the
C  origin at the lower left. Tickmarks with labels show the data
C  coordinate
C  range, and an informational label at the lower right gives
C  the reference vector value.

      call NhlFRLClear(srlist)
      call NhlFRLSetinteger(srlist,'vcVectorFieldData',dataid,ierr)

C ********* see resource file vc05.res ******
C       call NhlFRLSetstring(srlist,'vcMonoLineArrowColor','false',
C      1    ierr)
C       call NhlFRLSetstring(srlist,'vcMaxLevelCount',61,
C      1    ierr)
C 

      call NhlFCreate(vcid,'VectorPlot1',NhlFVectorPlotClass,wid,
     1    srlist,ierr)

C  Draw the 1st frame, signify end of 1st frame.

      call NhlFDraw(vcid,ierr)
      call NhlFFrame(wid,ierr)

C  Destroy the objects created for the 1st frame. 
C  Keep wid and appid for 2nd frame.

      call NhlFDestroy(dataid,ierr)
      call NhlFDestroy(vcid,ierr)

C  BEGIN CREATING 2nd FRAME

C  Create the 2nd VectorField data object using the same data.
C  Map the domain and range of the vector locations to the latitude
C  and longitude ranges of the world map on which the vectors
C  will be overlayed. Thus, cause the x axis locations to cover -180
C  to 180 degrees longitude, and cause the y axis locations to cover
C  -90 to 90 degrees latitude.
 
        call NhlFRLClear(srlist)
        call NhlFRLSetmdfloatArray(srlist,'vfUDataArray',U,2,
     1      len_dims,ierr)
        call NhlFRLSetmdfloatArray(srlist,'vfVDataArray',V,2,
     1      len_dims,ierr)

C ********* see resource file vc05.res ******   
C         call NhlFRLSetinteger(srlist,'vfXCStartV', -180,ierr)
C         call NhlFRLSetinteger(srlist,'vfXCEndV', 180,ierr)
C         call NhlFRLSetinteger(srlist,'vfYCStartV', -90,ierr)
C         call NhlFRLSetinteger(srlist,'vfYCEndV', 90,ierr)
C 

      call NhlFCreate(dataid,'increasingvectors2',NhlFVectorFieldClass,
     1      appid,srlist,ierr)


C  Create the 2nd VectorPlot object, as in the 1st frame.
C  Make the minimum vector length 1/3 of the reference vector length.

      call NhlFRLClear(srlist)
      call NhlFRLSetinteger(srlist,'vcVectorFieldData',dataid,ierr)

C ********* see resource file vc05.res ******
C       call NhlFRLSetstring(srlist,'vcMonoLineArrowColor','false',
C      1    ierr)
C       call NhlFRLSetstring(srlist,'vcMaxLevelCount',61,
C      1    ierr)
C       call NhlFRLSetfloat(srlist,'vcMinFracLengthF', .33,ierr)
C 

      call NhlFCreate(vcid,'VectorPlot2',NhlFVectorPlotClass,
     1      wid,srlist,ierr)
  

C  Create a mapPlot object consisting of, the default, gridded
C  world map.
C  Add a title object which says "vc05 frame 2".
C  Turn off the grid lines. 

      call NhlFRLClear(srlist)
      call NhlFRLSetstring(srlist,'pmTitleDisplayMode','always',ierr)
      call NhlFRLSetstring(srlist,'tiMainString','vc05 frame 2',
     1    ierr)

C********* see resource file vc05.res ******
C      call NhlFRLSetstring(srlist,'mpGridAndLimbOn', 'false',ierr)
C

      call NhlFCreate(mapid,'Map1',NhlFMapPlotClass,wid,srlist,ierr)

C  Overlay the VectorPlot2 VectorPlot object onto the Map1 MapPlot
C  object.

      call NhlFAddOverlay(mapid, vcid,-1,ierr)

C  Draw the 2nd frame. Signify end of 2nd frame.

 
      call NhlFDraw(mapid,ierr)
      call NhlFFrame(wid,ierr)

C  Destroy the objects created for 2nd frame. Keep wid and appid
C  for 3rd frame.

      call NhlFDestroy(dataid,ierr)
      call NhlFDestroy(vcid,ierr)
      call NhlFDestroy(mapid,ierr)

C  BEGIN CREATING 3rd FRAME

C  Create a data object from the same data.
C  Cause the x axis data locations to cover -180 to 180 degrees
C   longitude.
C  Cause the y axis data locations to cover 50 degrees latitude up
C  to the
C  north pole.

      call NhlFRLClear(srlist)
      call NhlFRLSetmdfloatArray(srlist,'vfUDataArray',U,2,
     1    len_dims,ierr)
      call NhlFRLSetmdfloatArray(srlist,'vfVDataArray',V,2,
     1    len_dims,ierr)

C********* see resource file vc05.res ******
C      call NhlFRLSetinteger(srlist,'vfXCStartV', -180,ierr)
C      call NhlFRLSetinteger(srlist,'vfXCEndV', 180,ierr)
C      call NhlFRLSetinteger(srlist,'vfYCStartV', 50,ierr)
C      call NhlFRLSetinteger(srlist,'vfYCEndV', 90,ierr)
C

      call NhlFCreate(dataid,'increasingvectors3',NhlFVectorFieldClass,
     1    appid,srlist,ierr)

C  Create a VectorPlot object, supplying the VectorField object as data,
C  as in frame 2. 
C  Double the default vector length in NDC. The default was to
C  dynamically
C  calulate it by dividing viewport width by the number of data values.
C  Display a horizontal label bar (at the bottom of the plot)
C  showing the color levels and their corresponding magnitudes.
C  Decrease the height of the label bar to .1 NDC so that it fits
C  into the
C  default viewPort.

        call NhlFRLClear(srlist)
      call NhlFRLSetinteger(srlist,'vcVectorFieldData',dataid,ierr)

C********* see resource file vc05.res ******
C      call NhlFRLSetstring(srlist,'vcMonoLineArrowColor','false',
C     1    ierr)
C      call NhlFRLSetstring(srlist,'vcMaxLevelCount',61,
C     1    ierr)
C      call NhlFRLSetfloat(srlist,'vcMinFracLengthF', .33,ierr)  
C      call NhlFRLSetfloat(srlist,'vcRefLengthF', .05,ierr)
C      call NhlFRLSetstring(srlist,'pmLabelBarDisplayMode','always',
C     1    ierr)
C      call NhlFRLSetstring(srlist,'pmLabelBarSide', 'bottom',ierr)
C      call NhlFRLSetstring(srlist,'lbOrientation','horizontal',ierr)
C      call  NhlFRLSetfloat(srlist,'pmLabelBarHeightF', .11,ierr)
C      call  NhlFRLSetfloat(srlist,'pmLabelBarWidthF', .6,ierr)
C

      call NhlFCreate(vcid,'VectorPlot3',NhlFVectorPlotClass,wid,
     1      srlist,ierr)

C  Create an orthographic map of the world, centered at the north pole,
C  making the points at all four of the frame edges be 50 degrees north
C  latitude. Make the center of the top edge of the frame be 180
C  degrees 
C  longitude, the center of the bottom edge of the frame correspond to
C  0 degrees longitude, the center of the left edge of the frame
C  correspond
C  to -90 degrees longitude,and the center of right edge of the frame
C  correspond to +90 degrees longitude.
C  Create a title object which says "vc05 frame 3".
C  Turn off the grid lines.

      call NhlFRLClear(srlist)
      call NhlFRLSetstring(srlist,'pmTitleDisplayMode','always',ierr)
      call NhlFRLSetstring(srlist,'tiMainString','vc05 frame 3',
     1    ierr)

C********* see resource file vc05.res ******
C      call NhlFRLSetstring(srlist,'mpProjection', 'Orthographic',
C    1    ierr)
C      call NhlFRLSetfloat(srlist,'mpCenterLatF', 90.0,ierr) 
C      call NhlFRLSetfloat(srlist,'mpCenterLonF', 0.0,ierr) 
C      call NhlFRLSetstring(srlist,'mpLimitMode', 'points',ierr)
C      call NhlFRLSetfloat(srlist,'mpBottomPointLatF', 50.0,ierr)
C      call NhlFRLSetfloat(srlist,'mpTopPointLatF', 50.0,ierr)
C      call NhlFRLSetfloat(srlist,'mpLeftPointLatF', 50.0,ierr)
C      call NhlFRLSetfloat(srlist,'mpRightPointLatF', 50.0,ierr)
C      call NhlFRLSetfloat(srlist,'mpBottomPointLonF', 0.0,ierr)
C      call NhlFRLSetfloat(srlist,'mpTopPointLonF', 180.0,ierr)
C      call NhlFRLSetfloat(srlist,'mpLeftPointLonF', 270.0,ierr)
C      call NhlFRLSetfloat(srlist,'mpRightPointLonF', 90.0,ierr)
C      call NhlFRLSetstring(srlist,'mpGridAndLimbOn', 'false',ierr)
C

      call NhlFCreate(mapid,'Map2',NhlFMapPlotClass,wid,srlist,ierr)

C  Overlay the VectorPlot3 VectorPlot object onto the Map2 MapPlot
C  object.


       call  NhlFAddOverlay(mapid, vcid,-1,ierr)


C  Draw the 3rd frame. Signify end of 3rd frame.

  
      call NhlFDraw(mapid,ierr)
      call NhlFFrame(wid,ierr)

C  Destroy all of the objects created, close the HLU library and exit.


      call NhlFDestroy(dataid,ierr)
      call NhlFDestroy(vcid,ierr)
      call NhlFDestroy(mapid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFDestroy(appid,ierr)

      call NhlFClose
      
      stop
      end


 
