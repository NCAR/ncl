C
C      $Id: pr04f.f,v 1.2 1998-02-11 18:56:17 dbrown Exp $
C
C***********************************************************************
C                                                                      *
C                Copyright (C)  1996                                   *
C        University Corporation for Atmospheric Research               *
C                All Rights Reserved                                   *
C                                                                      *
C***********************************************************************
C
C  File:       pr04f.f
C
C  Author:     David Brown (converted by Mary Haley)
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Fri Jun 28 11:06:23 MDT 1996
C
C  Description:    Demonstrates graphics primitives drawn in NDC and 
C                  and data space into an IrregularPlot (and therefore 
C                  using the IrregularTransformation).
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFXWorkstationClass
      external NhlFirregularPlotClass

      integer rlist,grlist
      integer appid,wid,canvas
      integer i 
      real plx(5),ply(5),pmx(4),pmy(4),pgx(4),pgy(4)
      real dpgx(7),dpgy(7)
      data plx/0.1,0.9,0.5,0.1,0./
      data ply/0.1,0.1,0.9,0.1,0./
      data pmx/0.05,0.95,0.5,0.5/
      data pmy/0.05,0.05,1.05,0.5/
      data pgx/0.2,0.8,0.5,0.2/
      data pgy/0.25,0.25,0.85,0.25/
      data dpgx/5.0,110.0,110.0,0.0,110.0,5.0,5.0/
      data dpgy/10.,10.,20.0,20.,110.,110.,10.0/
      integer NCGM, X11, PS

      NCGM=0
      X11=1
      PS=0
C
C Initialize the high level utility library
C
      call NhlFInitialize()
C
C Create an application context. Set the app dir to the current
C directory so the application looks for a resource file in the working
C directory. 
C
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLCreate(grlist,'GETRL')

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'pr04',NhlFappClass,
     1     0,rlist,ierr)

      if (NCGM.eq.1) then
C
C  Create a meta file workstation.
C   
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkMetaName','./pr04f.ncgm',ierr)
      call NhlFCreate(wid,'pr04Work',NhlFncgmWorkstationClass,
     1    0,rlist,ierr)
    
      else if (X11.eq.1) then
C
C  Create an X workstation.
C   
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'wkPause','true',ierr)
      call NhlFCreate(wid,'pr04Work',NhlFxWorkstationClass,
     1            appid,rlist,ierr)
    
      else if (PS.eq.1) then
C
C  Create a PS workstation.
C   
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkPSFileName','./pr04.ps',ierr)
      call NhlFCreate(wid,'pr04Work',NhFlpsWorkstationClass,
     1    appid,rlist,ierr)

      endif    
C
C Create an IrregularPlot that covers the entire NDC space 
C to use as a drawing canvas
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vpXF',0.1,ierr)
      call NhlFRLSetFloat(rlist,'vpYF',0.9,ierr)
      call NhlFRLSetFloat(rlist,'vpWidthF',0.8,ierr)
      call NhlFRLSetFloat(rlist,'vpHeightF',0.8,ierr)
      call NhlFRLSetString(rlist,'pmTitleDisplayMode','always',ierr)
      call NhlFRLSetString(rlist,'tiMainString',
     +     'Irregular Plot with NDC Primitives',ierr)
      call NhlFCreate(canvas,'canvas',NhlFirregularPlotClass,wid,rlist,
     +     ierr)

      call NhlFDraw(canvas,ierr)
      call NhlFNDCPolyline(canvas,0,plx,ply,4,ierr)
      call NhlFNDCPolygon(canvas,0,pgx,pgy,4,ierr)
      call NhlFNDCPolymarker(canvas,0,pmx,pmy,4,ierr)
      call NhlFFrame(wid,ierr)

      do 20 i=1,4
         plx(i) = plx(i) * 100.0 + 50.
         ply(i) = ply(i) * 100.0 + 50.
         pgx(i) = pgx(i) * 100.0 + 50.
         pgy(i) = pgy(i) * 100.0 + 50.
         pmx(i) = pmx(i) * 100.0 + 50.
         pmy(i) = pmy(i) * 100.0 + 50.
 20   continue

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     +     'Clipped Data Space Primitives',ierr)
      call NhlFSetValues(canvas,rlist,ierr)

      call NhlFDraw(canvas,ierr)
      call NhlFDataPolyline(canvas,0,plx,ply,4,ierr)
      call NhlFDataPolymarker(canvas,0,pmx,pmy,4,ierr)
      call NhlFDataPolygon(canvas,0,pgx,pgy,4,ierr)
      call NhlFFrame(wid,ierr)

      do 30 i=1,4
         plx(i) = plx(i) - 40.
         ply(i) = ply(i) - 40.
         pgx(i) = pgx(i) - 40.
         pgy(i) = pgy(i) - 40.
         pmx(i) = pmx(i) - 40.
         pmy(i) = pmy(i) - 40.
 30   continue

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     +     'Data Space Primitives Repositioned',ierr)
      call NhlFSetValues(canvas,rlist,ierr)

      call NhlFDraw(canvas,ierr)
      call NhlFDataPolyline(canvas,0,plx,ply,4,ierr)
      call NhlFDataPolymarker(canvas,0,pmx,pmy,4,ierr)
      call NhlFDataPolygon(canvas,0,pgx,pgy,4,ierr)
      call NhlFFrame(wid,ierr)


      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     +     'A Diamond in Data Space',ierr)
      call NhlFSetValues(canvas,rlist,ierr)
      call NhlFDraw(canvas,ierr)
      plx(1) = 10.0
      plx(2) = 50.0
      plx(3) = 90.0
      plx(4) = 50.0
      plx(5) = 10.0
      ply(1) = 50.0
      ply(2) = 10.0
      ply(3) = 50.0
      ply(4) = 90.0
      ply(5) = 50.0
      call NhlFDataPolygon(canvas,0,plx,ply,4,ierr)
      plx(1) = 5.0
      plx(3) = 95.0
      plx(5) = 5.0
      ply(2) = 5.0
      ply(4) = 95.0
      call NhlFDataPolyline(canvas,0,plx,ply,5,ierr)
      call NhlFFrame(wid,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     +     'A Self-Intersecting Data Polygon',ierr)
      call NhlFSetValues(canvas,rlist,ierr)
      call NhlFDraw(canvas,ierr)
      call NhlFDataPolygon(canvas,0,dpgx,dpgy,7,ierr)
      call NhlFFrame(wid,ierr)

      call NhlFClose()
      stop
      end
