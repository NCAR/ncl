C
C     $Id: an01f.f,v 1.1 1995-01-24 23:21:43 haley Exp $
C
C************************************************************************
C                                                                       *
C                            Copyright (C)  1995                        *
C                 University Corporation for Atmospheric Research       *
C                            All Rights Reserved                        *
C                                                                       *
C************************************************************************
C
C      File:            an01f.f
C
C      Author:          David Brown (converted to Fortran by Mary Haley)
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Tue Jan 24 10:49:54 MST 1995
C
C     Description:      Illustrates use of Annotation objects.    
C
      external nhlfapplayerclass
      external nhlfncgmworkstationlayerclass
      external nhlfmapplotlayerclass
      external nhlftextitemlayerclass
      external nhlfannotationlayerclass
C
C Define enough frames for a fairly smooth animation.
C
      parameter(FRAME_COUNT=36)

      character*50 name(25)
      real lat(25),lon(25)
      integer anno_id(25), obj_id(25)
      data anno_id/25*-1/
      data obj_id/25*-1/

      integer ret
      data ret/-1/
      integer appid,wid,mapid,rlist
      integer i
      character*256 buf

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
C Initialize the high level utility library
C
      call nhlfinitialize
C
C Create an application context. Set the app dir to the current directory
C so the application looks for a resource file in the working directory.
C The resource file sets most of the Contour resources that remain fixed
C throughout the life of the Contour object.
C
      call nhlfrlcreate(rlist,'SETRL')
      call nhlfrlclear(rlist)
      call nhlfrlsetstring(rlist,'appUsrDir','./',ierr)
      call nhlfcreate(appid,'an01',nhlfapplayerclass,0,rlist,ierr)
C
C Create a meta file workstation
C
      call nhlfrlclear(rlist)
      call nhlfrlsetstring(rlist,'wkMetaName','./an01f.ncgm',ierr)
      call nhlfcreate(wid,'an01Work',nhlfncgmworkstationlayerclass,0,
     1         rlist,ierr)
C
C Create a Map Plot object
C
      call nhlfrlclear(rlist)
      call nhlfrlsetstring(rlist,'ovTitleDisplayMode','always',ierr)
      call nhlfrlsetstring(rlist,'tiMainString','an01f',ierr)
      call nhlfrlsetstring(rlist,'mpFillOn','TRUE',ierr)
      call nhlfrlsetstring(rlist,'mpProjection','orthographic',ierr)
      call nhlfcreate(mapid,'Map0',nhlfmapplotlayerclass,wid,rlist,ierr)
C
C Annotation objects are generic object containers that the Overlay
C object knows how to manipulate in a uniform fashion. They may be 
C manipulated in NDC space like the Title or LabelBar objects, or, as
C in this example, aligned with with the plot object's data space.
C
C Create a TextItem for each place name to be included on the map.
C Then create an Annotation object for each TextItem. Register each
C Annotation with the MapPlot object, the creator of the Overlay.
C
      do 10 i = 1,25
         call nhlfrlclear(rlist)
         call nhlfrlsetstring(rlist,'txString',name(i),ierr)
         call nhlfrlsetfloat(rlist,'txFontHeightF',0.01,ierr)
         call nhlfrlsetinteger(rlist,'txFontColor',18,ierr)
         call nhlfcreate(obj_id(i),name(i),nhlftextitemlayerclass,wid,
     1    rlist,ierr)
         call nhlfrlclear(rlist)
         call nhlfrlsetstring(rlist,'anResizeNotify','TRUE',ierr)
         call nhlfrlsetstring(rlist,'anTrackData','TRUE',ierr)
         call nhlfrlsetstring(rlist,'anJust','centerleft',ierr)
         call nhlfrlsetfloat(rlist,'anDataXF',lon(i),ierr)
         call nhlfrlsetfloat(rlist,'anDataYF',lat(i),ierr)
         call nhlfrlsetinteger(rlist,'anPlotId',obj_id(i),ierr)
         call nhlfcreate(anno_id(i),name(i),nhlfannotationlayerclass,
     1        wid,rlist,ierr)
         call nhlfregisterannotation(mapid,anno_id(i),ierr)
 10   continue
C
C Create FRAME_COUNT plots, varying the center longitude by an equal
C amount each time.
C
      do 20 i = FRAME_COUNT,1,-1
         call nhlfrlclear(rlist)
         call nhlfrlsetfloat(rlist,'mpCenterLonF',i*360.0/FRAME_COUNT,
     1    ierr)
         call nhlfsetvalues(mapid,rlist,ierr)
         call nhlfdraw(mapid,ierr)
         call nhlfframe(wid,ierr)
 20   continue
C
C Destroy the objects created, close the HLU library and exit.
C
      do 30 i=1,25
         call nhlfdestroy(obj_id(i),ierr)
         call nhlfdestroy(anno_id(i),ierr)
 30   continue
      call nhlfdestroy(mapid,ierr)
      call nhlfdestroy(wid,ierr)
      call nhlfdestroy(appid,ierr)
      call nhlfclose
      stop
      end
