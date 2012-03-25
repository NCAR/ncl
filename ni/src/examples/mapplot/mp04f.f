CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                            Copyright (C)  1995                       C
C                 University Corporation for Atmospheric Research      C
C                            All Rights Reserved                       C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:            mp04f.f
C
C      Author:          David Brown (converted to Fortran by Mary Haley)
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Tue Jan 24 10:49:54 MST 1995
C
C     Description:      Illustrates use of AnnoManager objects.    
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFMapPlotClass
      external NhlFtextitemClass
      external nhlfannomanagerclass
C
C Define enough frames for a fairly smooth animation.
C
      parameter(IFRAME_COUNT=36,NDIM=25)

      character*50 name(NDIM)
      real lat(NDIM),lon(NDIM)
      integer num_am_ids
      data num_am_ids/NDIM/
      integer am_ids(NDIM), text_ids(NDIM)
      data am_ids/25*-1/

      integer ret
      data ret/-1/
      integer appid,wid,mapid,rlist,grlist
      integer i
      character*7  wks_type

      data name/'Los Angeles','Seattle','Toronto','New York','Miami',
     1 'Mexico City','London','Jakarta','Moscow','New Delhi',
     1 'Rio de Janeiro','Cairo','Buenos Aires','Beijing','Tokyo',
     1 'Lagos','Nairobi','Sydney','Bogota','Lima','Cape Town',
     1 'Calcutta','Shanghai','Bombay','Denver'/

      data lat/34.0,47.6,43.7,40.67,25.75,19.417,51.32,-6.13,55.75,
     1 28.37,-22.883,30.05, -34.67,39.917,35.67,6.45,-1.283,-33.9167,
     1 4.633,-12.1,-33.933,22.583,31.217,18.93,39.716/

      data lon/-118.28,-122.33,-79.4167,-73.83,-80.25,-99.167,-0.1,
     1 106.75,37.7,77.217,-43.283,31.25,-58.4167,116.4167,139.67,3.28,
     1 36.833,151.167,-74.083,-77.05,18.4667,88.35,121.4167,72.85,
     1 -105.017/

C
C Default is to create a metafile.
C
      wks_type = "ncgm"

C
C Initialize the high level utility library
C
      call NhlFInitialize
C
C Create an application context. Set the app dir to the current
C directory so the application looks for a resource file in the
C working directory. The resource file sets most of the Contour
C resources that remain fixed throughout the life of the Contour
C object.
C
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFRLSetstring(rlist,'appDefaultParent','True',ierr)
      call NhlFCreate(appid,'mp04',NhlFAppClass,0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create a meta file workstation
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./mp04f.ncgm',ierr)
         call NhlFCreate(wid,'mp04Work',NhlFNcgmWorkstationClass,
     1        0,rlist,ierr)
      else  if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X workstation
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'mp04Work',
     1        NhlFCairoWindowWorkstationClass,0,
     1        rlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./mp04f.ps',ierr)
         call NhlFCreate(wid,'mp04Work',NhlFPSWorkstationClass,
     1        0,rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPDFFileName','./mp04f.pdf',ierr)
         call NhlFCreate(wid,'mp04Work',NhlFPDFWorkstationClass,
     1        0,rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetstring(rlist,'wkFileName','./mp04f',ierr)
         call NhlFCreate(wid,'mp04Work',
     1        NhlFCairoPSPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetstring(rlist,'wkFileName','./mp04f',ierr)
         call NhlFCreate(wid,'mp04Work',
     1        NhlFCairoImageWorkstationClass,0,rlist,ierr)
      endif
C
C AnnoManager objects allow the PlotManager to manipulate any View class
C object as an annotation a uniform fashion. They allow
C the user to set the View object's size and location relative to
C the viewport of a Plot. They may be located relative to one
C of the viewport sides, or, as in this example, aligned with the 
C plot's data space (amTrackData is set True in the resource file).
C
C Create a TextItem for each place name to be included on the map.
C Collect the object ids into an array.
C
      do 10 i = 1,NDIM
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'txString',name(i),ierr)
         call NhlFCreate(text_ids(i),name(i),NhlFtextitemClass,wid,
     1    rlist,ierr)
 10   continue
C
C Since the MapPlot object is by default a PlotManager, you can
C make each TextItem View object into an annotation simply by setting
C the pmAnnoViews resource with the array of TextItem ids. 
C
      call NhlFRLClear(rlist)
      call NhlFRLSetintegerarray(rlist,'pmAnnoViews',text_ids,NDIM,ierr)
      call NhlFCreate(mapid,'Map0',NhlFMapPlotClass,wid,rlist,ierr)
C
C Retrieve the ids of the AnnoManager objects created by the PlotManager
C and then set their location in data coordinate space. The AnnoManager
C objects are arranged in the same order as the TextItems in the
C pmAnnoViews resource.
C
      call NhlFRLCreate(grlist,'GETRL')
      call NhlFRLClear(grlist)
      call NhlFRLGetintegerarray(grlist,'pmAnnoManagers',am_ids,
     +                           num_am_ids,ierr)
      call NhlFGetValues(mapid,grlist,ierr)

      do 20 i=1,num_am_ids
         call NhlFRLClear(rlist)
         call NhlFRLSetfloat(rlist,'amDataXF',lon(i),ierr)
         call NhlFRLSetfloat(rlist,'amDataYF',lat(i),ierr)
         call NhlFSetValues(am_ids(i),rlist,ierr)
 20   continue
C
C Create IFRAME_COUNT plots, varying the center longitude by an equal
C amount each time.
C
      do 30 i = IFRAME_COUNT,1,-1
         call NhlFRLClear(rlist)
         call NhlFRLSetfloat(rlist,'mpCenterLonF',i*360.0/IFRAME_COUNT,
     1    ierr)
         call NhlFSetValues(mapid,rlist,ierr)
         call NhlFDraw(mapid,ierr)
         call NhlFFrame(wid,ierr)
 30   continue
C
C Destroy the objects created, close the HLU library and exit.
C
      do 40 i=1,NDIM
         call NhlFDestroy(text_ids(i),ierr)
 40   continue
      call NhlFDestroy(mapid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFDestroy(appid,ierr)
      call NhlFClose
      stop
      end
