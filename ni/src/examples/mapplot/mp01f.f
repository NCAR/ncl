C
C     $Id: mp01f.f,v 1.7 1995-04-07 10:54:20 boote Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1993                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C      File:            mp01f.f
C
C      Author:          Dave Brown (converted to Fortran by Mary Haley)
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Tue Jan 24 10:08:49 MST 1995
C
C      Description:     Demonstrates basic MapPlot capabilities.
C
      external NhlFAppClass
      external NhlFXWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFMapPlotClass
      integer appid,wid,mapid
      integer rlist
      integer NCGM
C
C Default is to display output to an X workstation
C
      NCGM=0
C
C Initialize the high level utility library
C
      call NhlFInitialize
C
C Create an application context. Set the app dir to the current
C directory so the application looks for a resource file in the working
C directory. The resource file sets most of the Contour resources that
C remain fixed throughout the life of the Contour object.
C
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'mp01',NhlFAppClass,0,rlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./mp01f.ncgm',ierr)
         call NhlFCreate(wid,'mp01Work',NhlFNcgmWorkstationClass,0,
     1        rlist,ierr)
      else 
C
C Create an X workstation
C
         call NhlFRLClear(rlist)
         call NhlFRLSetinteger(rlist,'wkPause',1,ierr)
         call NhlFCreate(wid,'mp01Work',NhlFXWorkstationClass,0,
     1     rlist,ierr)
      endif
C
C Draw the default MapPlot object
C
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'pmTitleDisplayMode','always',ierr)
      call NhlFRLSetstring(rlist,'tiMainString','mp01f - Frame 1',ierr)
      call NhlFCreate(mapid,'Map0',NhlFMapPlotClass,wid,rlist,ierr)
      call NhlFDraw(mapid,ierr)
      call NhlFFrame(wid,ierr)
C
C Change some projection resources, add color fill, and
C all the outlines (Geophysical, National, and US States).
C
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'tiMainString','mp01f - Frame 2',ierr)
      call NhlFRLSetfloat(rlist,'vpYF',0.85,ierr)
      call NhlFRLSetfloat(rlist,'vpXF',0.15,ierr)
      call NhlFRLsetfloat(rlist,'vpHeightF',0.7,ierr)
      call NhlFRLsetfloat(rlist,'vpWidthF',0.7,ierr)
      call NhlFRLSetstring(rlist,'mpFillOn','TRUE',ierr)
      call NhlFRLSetstring(rlist,'mpOutlineBoundarySets',
     1  'allBoundaries',ierr)
      call NhlFRLSetstring(rlist,'mpProjection','orthographic',ierr)
      call NhlFRLSetfloat(rlist,'mpCenterLatF',10.0,ierr)
      call NhlFRLSetfloat(rlist,'mpCenterLonF',-90.0,ierr)
      call NhlFRLSetfloat(rlist,'mpCenterRotF',45.0,ierr)
      call NhlFSetValues(mapid,rlist,ierr)

      call NhlFDraw(mapid,ierr)
      call NhlFFrame(wid,ierr)
C
C Use the national color set and limit the projection, 
C using lat/lon boundaries.
C

      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'tiMainString','mp01f - Frame 3',ierr)
      call NhlFRLSetstring(rlist,'mpFillBoundarySets','national',ierr)
      call NhlFRLSetstring(rlist,'mpLimitMode','latlon',ierr)
      call NhlFRLSetfloat(rlist,'mpMinLatF',-60.0,ierr)
      call NhlFRLSetfloat(rlist,'mpMaxLatF',60.0,ierr)
      call NhlFRLSetfloat(rlist,'mpMinLonF',-135.0,ierr)
      call NhlFRLSetfloat(rlist,'mpMaxLonF',-45.0,ierr)
      call NhlFSetValues(mapid,rlist,ierr)

      call NhlFDraw(mapid,ierr)
      call NhlFFrame(wid,ierr)
C
C Polar stereographic projection, change the grid spacing to 10 degrees
C

      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'tiMainString','mp01f - Frame 4',ierr)
      call NhlFRLSetstring(rlist,'mpProjection','stereographic',ierr)
      call NhlFRLSetfloat(rlist,'mpGridSpacingF',10.,ierr)
      call NhlFRLSetfloat(rlist,'mpMinLatF',20.0,ierr)
      call NhlFRLSetfloat(rlist,'mpMaxLatF',90.0,ierr)
      call NhlFRLSetfloat(rlist,'mpMinLonF',0.0,ierr)
      call NhlFRLSetfloat(rlist,'mpMaxLonF',360.0,ierr)
      call NhlFRLSetfloat(rlist,'mpCenterLatF',90.0,ierr)
      call NhlFSetValues(mapid,rlist,ierr)

      call NhlFDraw(mapid,ierr)
      call NhlFFrame(wid,ierr)

C
C Satellite projection using the angle limit method
C color US States only individually.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'tiMainString','mp01f - Frame 5',ierr)
      call NhlFRLSetstring(rlist,'mpFillBoundarySets',
     1       'geophysicalAndUSStates',ierr)
      call NhlFRLSetstring(rlist,'mpProjection','satellite',ierr)
      call NhlFRLSetstring(rlist,'mpLimitMode','angles',ierr)
      call NhlFRLSetfloat(rlist,'mpLeftAngleF',45.0,ierr)
      call NhlFRLSetfloat(rlist,'mpRightAngleF',45.0,ierr)
      call NhlFRLSetfloat(rlist,'mpBottomAngleF',45.0,ierr)
      call NhlFRLSetfloat(rlist,'mpTopAngleF',45.0,ierr)
      call NhlFRLSetfloat(rlist,'mpCenterLatF',20.0,ierr)
      call NhlFRLSetfloat(rlist,'mpSatelliteDistF',1.75,ierr)
      call NhlFSetValues(mapid,rlist,ierr)

      call NhlFDraw(mapid,ierr)
      call NhlFFrame(wid,ierr)
C
C Destroy the objects created, close the HLU library and exit.
C
      call NhlFDestroy(mapid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFDestroy(appid,ierr)
      call NhlFClose
      stop
      end
