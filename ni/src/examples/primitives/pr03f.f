C
C      $Id: pr03f.f,v 1.2 1997-05-22 17:08:44 haley Exp $
C
C***********************************************************************
C                                                                      *
C                Copyright (C)  1996                                   *
C        University Corporation for Atmospheric Research               *
C                All Rights Reserved                                   *
C                                                                      *
C***********************************************************************
C
C  File:       pr03f.c
C
C  Author:     David Brown
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Mon May 13 14:00:21 MDT 1996
C
C  Description:    Demonstrates graphics primitives drawn in NDC space
C                  Many of the primitives are clipped in order to test
C                  the way clipping works.
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFXWorkstationClass
      external NhlFLogLinPlotClass
      external NhlFGraphicStyleClass

      integer rlist,grlist
      integer appid,wid,canvas,gsid,id
      integer i 
      real plx(4),ply(4),pmx(4),pmy(4),pgx(4),pgy(4)
      real dpgx(7),dpgy(7)
      data plx/0.1,0.9,0.5,0.1/
      data ply/0.1,0.1,0.9,0.1/
      data pmx/0.05,0.95,0.5,0.5/
      data pmy/0.05,0.05,1.05,0.5/
      data pgx/0.2,0.8,0.5,0.2/
      data pgy/0.25,0.25,0.85,0.25/
      data dpgx/10.0,110.0,110.0,5.0,110.0,10.0,10.0/
      data dpgy/1.,1.,20.0,20.,110.,110,1.0/
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
      call NhlFCreate(appid,'pr03',NhlFappClass,
     1     0,rlist,ierr)


      if (NCGM.eq.1) then
C
C  Create a meta file workstation.
C   
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkMetaName','./pr03f.ncgm',ierr)
      call NhlFCreate(wid,'pr03Work',NhlFncgmWorkstationClass,
     1    0,rlist,ierr)
    
      else if (X11.eq.1) then
C
C  Create an X workstation.
C   
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'wkPause','true',ierr)
      call NhlFCreate(wid,'pr03Work',NhlFxWorkstationClass,
     1            appid,rlist,ierr)
    
      else if (PS.eq.1) then
C
C  Create a PS workstation.
C   
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkPSFileName','./pr03.ps',ierr)
      call NhlFCreate(wid,'pr03Work',NhFlpsWorkstationClass,
     1    appid,rlist,ierr)

      endif    
C
C Get the workstation default GraphicStyle and change the value of
C one of its resources
C
      call NhlFRLGetInteger(grlist,'wkDefGraphicStyleId',id,ierr)
      call NhlFGetValues(wid,grlist,ierr)
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'gsLineColor',2,ierr)
      call NhlFSetValues(id,rlist,ierr)
C
C Create a LogLinPlot that covers the entire NDC space 
C to use as a drawing canvas
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vpXF',0.0,ierr)
      call NhlFRLSetFloat(rlist,'vpYF',1.0,ierr)
      call NhlFRLSetFloat(rlist,'vpWidthF',1.0,ierr)
      call NhlFRLSetFloat(rlist,'vpHeightF',1.0,ierr)
      call NhlFCreate(canvas,'canvas',NhlFlogLinPlotClass,wid,rlist,
     1     ierr)
C
C Explicitly create a GraphicStyle
C
      call NhlFRLClear(rlist)
      call NhlFCreate(gsid,'style',NhlFgraphicStyleClass,wid,rlist,
     1     ierr)
C
C Set workstation line attributes and draw a triangle
C
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'gsLineDashPattern',2,ierr)
      call NhlFRLSetFloat(rlist,'gsLineThicknessF',3.0,ierr)
      call NhlFSetValues(gsid,rlist,ierr)

      call NhlFDraw(canvas,ierr)
C
C The polyline call uses the default GraphicStyle. Polygon and
C Polymarker use the explicitly created GraphicStyle.
C
      call NhlFNDCPolyline(canvas,0,plx,ply,4,ierr)
      call NhlFNDCPolygon(canvas,gsid,pgx,pgy,4,ierr)
      call NhlFNDCPolymarker(canvas,gsid,pmx,pmy,4,ierr)
      call NhlFFrame(wid,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vpXF',0.2,ierr)
      call NhlFRLSetFloat(rlist,'vpYF',0.8,ierr)
      call NhlFRLSetFloat(rlist,'vpWidthF',0.6,ierr)
      call NhlFRLSetFloat(rlist,'vpHeightF',0.6,ierr)
      call NhlFSetValues(canvas,rlist,ierr)
       
      call NhlFDraw(canvas,ierr)
      call NhlFNDCPolyline(canvas,gsid,plx,ply,4,ierr)
      call NhlFNDCPolygon(canvas,gsid,pgx,pgy,4,ierr)
      call NhlFNDCPolymarker(canvas,gsid,pmx,pmy,4,ierr)
      call NhlFFrame(wid,ierr)

      do 10 i=1,4
         plx(i) = plx(i) + 0.4
         ply(i) = ply(i) + 0.4
         pgx(i) = pgx(i) + 0.4
         pgy(i) = pgy(i) + 0.4
         pmx(i) = pmx(i) + 0.4
         pmy(i) = pmy(i) + 0.4
 10   continue
       
      call NhlFDraw(canvas,ierr)
      call NhlFNDCPolyline(canvas,gsid,plx,ply,4,ierr)
      call NhlFNDCPolygon(canvas,gsid,pgx,pgy,4,ierr)
      call NhlFNDCPolymarker(canvas,gsid,pmx,pmy,4,ierr)
      call NhlFFrame(wid,ierr)

      do 20 i=1,4
         plx(i) = plx(i) - 0.4
         ply(i) = ply(i) - 0.4
         pgx(i) = pgx(i) - 0.4
         pgy(i) = pgy(i) - 0.4
         pmx(i) = pmx(i) - 0.4
         pmy(i) = pmy(i) - 0.4
 20      continue
C
C Modify the data values to be in a meaningful range relative to
C the data coordinate extent.
C
      do 30 i=1,4
         plx(i) = plx(i) * 100.0 + 50.
         ply(i) = ply(i) * 100.0 + 50.
         pgx(i) = pgx(i) * 100.0 + 50.
         pgy(i) = pgy(i) * 100.0 + 50.
         pmx(i) = pmx(i) * 100.0 + 50.
         pmy(i) = pmy(i) * 100.0 + 50.
 30   continue

      call NhlFDraw(canvas,ierr)
      call NhlFDataPolyline(canvas,gsid,plx,ply,4,ierr)
      call NhlFDataPolymarker(canvas,gsid,pmx,pmy,4,ierr)
      call NhlFDataPolygon(canvas,gsid,pgx,pgy,4,ierr)
      call NhlFFrame(wid,ierr)

      do 40 i=1,4
         plx(i) = plx(i) - 80.
         ply(i) = ply(i) - 80.
         pgx(i) = pgx(i) - 80.
         pgy(i) = pgy(i) - 80.
         pmx(i) = pmx(i) - 80.
         pmy(i) = pmy(i) - 80.
 40   continue

      call NhlFDraw(canvas,ierr)
      call NhlFDataPolyline(canvas,gsid,plx,ply,4,ierr)
      call NhlFDataPolymarker(canvas,gsid,pmx,pmy,4,ierr)
      call NhlFDataPolygon(canvas,gsid,pgx,pgy,4,ierr)
      call NhlFFrame(wid,ierr)

      call NhlFDraw(canvas,ierr)
      call NhlFDataPolygon(canvas,gsid,dpgx,dpgy,7,ierr)
      call NhlFFrame(wid,ierr)

      call NhlFClose()
      stop
      end
