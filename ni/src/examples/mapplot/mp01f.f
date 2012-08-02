C
C     $Id: mp01f.f,v 1.12 2010-03-15 22:49:24 haley Exp $
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
      external NhlFCairoWindowWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFMapPlotClass
      integer appid,wid,mapid
      integer rlist
      character*7  wks_type
C
C Default is to display output to an X workstation
C
      wks_type = "x11"
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

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./mp01f.ncgm',ierr)
         call NhlFCreate(wid,'mp01Work',NhlFNcgmWorkstationClass,0,
     1        rlist,ierr)
      else  if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X workstation
C
         call NhlFRLClear(rlist)
         call NhlFRLSetinteger(rlist,'wkPause',1,ierr)
         call NhlFCreate(wid,'mp01Work',
     +     NhlFCairoWindowWorkstationClass,0,
     1     rlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./mp01f.ps',ierr)
         call NhlFCreate(wid,'mp01Work',NhlFPSWorkstationClass,0,
     1        rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPDFFileName','./mp01f.pdf',ierr)
         call NhlFCreate(wid,'mp01Work',NhlFPDFWorkstationClass,0,
     1        rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetstring(rlist,'wkFileName','./mp01f',ierr)
         call NhlFCreate(wid,'mp01Work',
     1        NhlFCairoPSPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetstring(rlist,'wkFileName','./mp01f',ierr)
         call NhlFCreate(wid,'mp01Work',
     1        NhlFCairoImageWorkstationClass,0,rlist,ierr)
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
      call NhlFRLSetfloat(rlist,'vpYF',0.9,ierr)
      call NhlFRLsetfloat(rlist,'vpHeightF',0.8,ierr)
      call NhlFRLSetstring(rlist,'mpFillOn','TRUE',ierr)
      call NhlFRLSetstring(rlist,'mpOutlineBoundarySets',
     1  'allBoundaries',ierr)
      call NhlFRLSetstring(rlist,'mpProjection','orthographic',ierr)
      call NhlFRLSetString(rlist,'mpPerimOn','true',ierr)
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
