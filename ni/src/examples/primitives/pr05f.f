C
C      $Id: pr05f.f,v 1.2 1997-02-05 15:26:02 haley Exp $
C
C***********************************************************************
C                                                                      *
C                Copyright (C)  1996                                   *
C        University Corporation for Atmospheric Research               *
C                All Rights Reserved                                   *
C                                                                      *
C***********************************************************************
C
C  File:       pr05f.f
C
C  Author:     David Brown (converted by Mary Haley)
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Fri Jun 28 13:52:30 MDT 1996
C
C  Description:    Demonstrates graphics primitives drawn in data
C                  space over a MapPlot. Data polygons are used to
C                  color global zones and data polylines mark the
C                  zonal boundaries. Line labels name the boundary
C                  lines (e.g. arctic circle). Markers are drawn at
C                  the poles. The three frames present three 
C                  different map projections, showing how the 
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFXWorkstationClass
      external NhlFMapPlotClass
      external NhlFGraphicStyleClass

      integer rlist
      integer appid,wid,canvas,gsid
      integer i 
      real px(5),py(5)
      character*13 projection(3)
      data projection/'orthographic','mollweide','stereographic'/
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

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'pr05',NhlFappClass,
     1     0,rlist,ierr)

      if (NCGM.eq.1) then
C
C  Create a meta file workstation.
C   
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName','./pr05f.ncgm',ierr)
         call NhlFCreate(wid,'pr05Work',NhlFncgmWorkstationClass,
     1        0,rlist,ierr)
    
      else if (X11.eq.1) then
C
C  Create an X workstation.
C   
         call NhlFRLClear(rlist)
         call NhlFRLSetInteger(rlist,'wkPause','true',ierr)
         call NhlFCreate(wid,'pr05Work',NhlFxWorkstationClass,
     1        appid,rlist,ierr)
    
      else if (PS.eq.1) then
C
C  Create a PS workstation.
C   
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPSFileName','./pr05.ps',ierr)
         call NhlFCreate(wid,'pr05Work',NhFlpsWorkstationClass,
     1        appid,rlist,ierr)

      endif    
C
C Create a MapPlot that covers the entire NDC space 
C to use as a drawing canvas
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vpXF',0.0,ierr)
      call NhlFRLSetFloat(rlist,'vpYF',1.0,ierr)
      call NhlFRLSetFloat(rlist,'vpWidthF',1.0,ierr)
      call NhlFRLSetFloat(rlist,'vpHeightF',1.0,ierr)
      call NhlFCreate(canvas,'canvas',NhlFmapPlotClass,wid,rlist,ierr)
C
C Create a GraphicStyle to control the primitive attributes.
C
      call NhlFCreate(gsid,'style',NhlFgraphicStyleClass,wid,0,ierr)

      do 10 i=1,3
C
C Set the map projection
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'mpProjection',projection(i),ierr)
         call NhlFSetValues(canvas,rlist,ierr)
C
C Draw polygons representing the 5 major zones of the globe, beginning
C with the tropical zone.
C Turn edges off and set the marker color.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetInteger(rlist,'gsMarkerColor',0,ierr)
         call NhlFRLSetString(rlist,'gsEdgesOn','false',ierr)
         call NhlFRLSetInteger(rlist,'gsFillColor',2,ierr)
         call NhlFSetValues(gsid,rlist,ierr)

         py(1) = -23.5
         py(2) = 23.5
         py(3) = 23.5
         py(4) = -23.5
         py(5) = -23.5
         px(1) = 360.
         px(2) = 360.
         px(3) = 0.
         px(4) = 0.
         px(5) = 360.
         call NhlFDataPolygon(canvas,gsid,px,py,5,ierr)
C
C Next draw the north and south temperate zones
C
         call NhlFRLClear(rlist)
         call NhlFRLSetInteger(rlist,'gsFillColor',3,ierr)
         call NhlFSetValues(gsid,rlist,ierr)

         py(1) = 23.5
         py(2) = 66.5
         py(3) = 66.5
         py(4) = 23.5
         py(5) = 23.5
         px(1) = 360.
         px(2) = 360.
         px(3) =  0.
         px(4) = 0.
         px(5) = 360.
         call NhlFDataPolygon(canvas,gsid,px,py,5,ierr)

         py(1) = -23.5
         py(2) = -66.5
         py(3) = -66.5
         py(4) = -23.5
         py(5) = -23.5
         px(1) = 360.
         px(2) = 360.
         px(3) =  0.
         px(4) = 0.
         px(5) = 360.
         call NhlFDataPolygon(canvas,gsid,px,py,5,ierr)
C
C Draw the frigid zones
C
         call NhlFRLClear(rlist)
         call NhlFRLSetInteger(rlist,'gsFillColor',4,ierr)
         call NhlFSetValues(gsid,rlist,ierr)

         py(1) = 90.
         py(2) = 66.5
         py(3) = 66.5
         py(4) = 90.
         py(5) = 90.
         px(1) = 360.
         px(2) = 360.
         px(3) =  0.
         px(4) = 0.
         px(5) = 360.
         call NhlFDataPolygon(canvas,gsid,px,py,5,ierr)

         py(1) = -90.
         py(2) = -66.5
         py(3) = -66.5
         py(4) = -90.
         py(5) = -90.
         px(1) = 360.
         px(2) = 360.
         px(3) =  0.
         px(4) = 0.
         px(5) = 360.
         call NhlFDataPolygon(canvas,gsid,px,py,5,ierr)
C
C Draw the map outlines and grid
C
         call NhlFDraw(canvas,ierr)
C
C Draw markers at each pole
C
         px(1) = 0
         px(2) = 0
         py(1) = 90
         py(2) = -90
         call NhlFDataPolymarker(canvas,gsid,px,py,2,ierr)
C
C Draw polylines at each of the major latitudinal boundary lines,
C beginning with the equator. Use the line label to name each of the
C lines. The '|' character is inserted between each label character 
C to allow the labels to track the curve of each line more precisely.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'gsLineLabelString','e|q|u|a|t|o|r',
     +        ierr)
         call NhlFSetValues(gsid,rlist,ierr)

         px(1) = 360
         px(2) = 0
         py(1) = 0
         py(2) = 0
         call NhlFDataPolyline(canvas,gsid,px,py,2,ierr)
C
C Tropic of cancer
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'gsLineLabelString',
     +        't|r|o|p|i|c o|f c|a|n|c|e|r',ierr)
         call NhlFSetValues(gsid,rlist,ierr)

         px(1) = 360
         px(2) = 0
         py(1) = 23.5
         py(2) = 23.5
         call NhlFDataPolyline(canvas,gsid,px,py,2,ierr)
C
C Tropic of capricorn (Note: currently there is a limit on the 
C number of characters in a line label that prevents the '|'
C character from being used between each letter in a label 
C of this length).
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'gsLineLabelString',
     +        'tr|o|p|ic of c|a|p|r|i|c|o|rn',ierr)
         call NhlFSetValues(gsid,rlist,ierr)

         px(1) = 360.
         px(2) = 0.
         py(1) = -23.5
         py(2) = -23.5
         call NhlFDataPolyline(canvas,gsid,px,py,2,ierr)
C
C Arctic circle
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'gsLineLabelString',
     +        'a|r|c|t|i|c c|i|r|c|l|e',ierr)
         call NhlFSetValues(gsid,rlist,ierr)

         px(1) = 360.
         px(2) = 0.
         py(1) = 66.5
         py(2) = 66.5
         call NhlFDataPolyline(canvas,gsid,px,py,2,ierr)
C
C Antarctic circle
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'gsLineLabelString',
     +        '|a|n|t|a|r|c|t|i|c c|i|r|c|l|e',ierr)
         call NhlFSetValues(gsid,rlist,ierr)

         px(1) = 360.
         px(2) = 0.
         py(1) = -66.5
         py(2) = -66.5
         call NhlFDataPolyline(canvas,gsid,px,py,2,ierr)

         call NhlFFrame(wid,ierr)
 10   continue
      
      call NhlFClose
      stop
      end
