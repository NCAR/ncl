C
C     $Id: mp01f.f,v 1.2 1995-01-25 22:07:24 haley Exp $
C
C************************************************************************
C                                                                       *
C                            Copyright (C)  1995                        *
C                 University Corporation for Atmospheric Research       *
C                            All Rights Reserved                        *
C                                                                       *
C************************************************************************
C
C      File:            mp01f.f
C
C      Author:          Dave Brown (converted to Fortran by Mary Haley)
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Tue Jan 24 10:08:49 MST 1995
C
C      Description:    	Demonstrates basic MapPlot capabilities.
C
      external nhlfapplayerclass
      external nhlfxworkstationlayerclass
      external nhlfmapplotlayerclass
      integer appid,wid,mapid
      integer rlist
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
      call nhlfcreate(appid,'mp01',nhlfapplayerclass,0,rlist,ierr)
C
C Create an X workstation
C
      call nhlfcreate(wid,'mp01Work',nhlfxworkstationlayerclass,0,
     1     0,ierr)
C
C Draw the default MapPlot object
C >>> Note that currently the MapPlot object does not have any means to
C >>> enforce the aspect ratio. Hopefully that will be changed before
C >>> the 4.0 release. For now adjust the window to create the proper
C >>> aspect ratio
C
      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'vpYF',0.775,ierr)
      call nhlfrlsetfloat(rlist,'vpHeightF',0.45,ierr)
      call nhlfrlsetstring(rlist,'ovTitleDisplayMode','always',ierr)
      call nhlfrlsetstring(rlist,'tiMainString','mp01f - Frame 1',ierr)
      call nhlfcreate(mapid,'Map0',nhlfmapplotlayerclass,wid,rlist,ierr)
      call nhlfdraw(mapid,ierr)
      call nhlfframe(wid,ierr)
C
C Change some projection resources, add color fill, and
C all the outlines (Geophysical, National, and US States).
C
      call nhlfrlclear(rlist)
      call nhlfrlsetstring(rlist,'tiMainString','mp01f - Frame 2',ierr)
      call nhlfrlsetfloat(rlist,'vpYF',0.9,ierr)
      call nhlfrlsetfloat(rlist,'vpHeightF',0.8,ierr)
      call nhlfrlsetstring(rlist,'mpFillOn','TRUE',ierr)
      call nhlfrlsetstring(rlist,'mpOutlineBoundarySets',
     1  'allBoundaries',ierr) 
      call nhlfrlsetstring(rlist,'mpProjection','orthographic',ierr)
      call nhlfrlsetfloat(rlist,'mpCenterLatF',10.0,ierr)
      call nhlfrlsetfloat(rlist,'mpCenterLonF',-90.0,ierr)
      call nhlfrlsetfloat(rlist,'mpCenterRotF',45.0,ierr)
      call nhlfsetvalues(mapid,rlist,ierr)

      call nhlfdraw(mapid,ierr)
      call nhlfframe(wid,ierr)
C
C Use the national color set and limit the projection, 
C using lat/lon boundaries.
C

      call nhlfrlclear(rlist)
      call nhlfrlsetstring(rlist,'tiMainString','mp01f - Frame 3',ierr)
      call nhlfrlsetstring(rlist,'mpFillBoundarySets','national',ierr)
      call nhlfrlsetstring(rlist,'mpMapLimitMode','latlon',ierr)
      call nhlfrlsetfloat(rlist,'mpMinLatF',-60.0,ierr)
      call nhlfrlsetfloat(rlist,'mpMaxLatF',60.0,ierr)
      call nhlfrlsetfloat(rlist,'mpMinLonF',-135.0,ierr)
      call nhlfrlsetfloat(rlist,'mpMaxLonF',-45.0,ierr)
      call nhlfsetvalues(mapid,rlist,ierr)

      call nhlfdraw(mapid,ierr)
      call nhlfframe(wid,ierr)
C
C Polar stereographic projection, change the grid spacing to 10 degrees
C

      call nhlfrlclear(rlist)
      call nhlfrlsetstring(rlist,'tiMainString','mp01f - Frame 4',ierr)
      call nhlfrlsetstring(rlist,'mpProjection','stereographic',ierr)
      call nhlfrlsetfloat(rlist,'mpGridSpacingF',10.,ierr)
      call nhlfrlsetfloat(rlist,'mpMinLatF',20.0,ierr)
      call nhlfrlsetfloat(rlist,'mpMaxLatF',90.0,ierr)
      call nhlfrlsetfloat(rlist,'mpMinLonF',0.0,ierr)
      call nhlfrlsetfloat(rlist,'mpMaxLonF',360.0,ierr)
      call nhlfrlsetfloat(rlist,'mpCenterLatF',90.0,ierr)
      call nhlfsetvalues(mapid,rlist,ierr)

      call nhlfdraw(mapid,ierr)
      call nhlfframe(wid,ierr)

C
C Satellite projection using the angle limit method
C color US States only individually.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetstring(rlist,'tiMainString','mp01f - Frame 5',ierr)
      call nhlfrlsetstring(rlist,'mpFillBoundarySets',
     1       'geophysicalAndUSStates',ierr)
      call nhlfrlsetstring(rlist,'mpProjection','satellite',ierr)
      call nhlfrlsetstring(rlist,'mpMapLimitMode','angles',ierr)
      call nhlfrlsetfloat(rlist,'mpLeftAngleF',45.0,ierr)
      call nhlfrlsetfloat(rlist,'mpRightAngleF',45.0,ierr)
      call nhlfrlsetfloat(rlist,'mpBottomAngleF',45.0,ierr)
      call nhlfrlsetfloat(rlist,'mpTopAngleF',45.0,ierr)
      call nhlfrlsetfloat(rlist,'mpCenterLatF',20.0,ierr)
      call nhlfrlsetfloat(rlist,'mpSatelliteDistF',1.75,ierr)
      call nhlfsetvalues(mapid,rlist,ierr)

      call nhlfdraw(mapid,ierr)
      call nhlfframe(wid,ierr)
C
C Destroy the objects created, close the HLU library and exit.
C
      call nhlfdestroy(mapid,ierr)
      call nhlfdestroy(wid,ierr)
      call nhlfdestroy(appid,ierr)
      call nhlfclose
      stop
      end
