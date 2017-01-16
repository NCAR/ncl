C
C      $Id: pr05f.f,v 1.6 2010-03-15 22:49:24 haley Exp $
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
C Note: beginning with NCL 6.2.0 the rules for drawing polylines 
C       (and to some extent polygons) changed.
C       Prior to 6.2.0 you could draw a polyline that spanned the 
C       globe using only 2 points with longitudes 0 and 360.
C       However, this created difficult to resolve ambiguities when
C       dealing with lines that cross the cyclic point of
C       longitude. Therefore, the rule was adapted that the path
C       between two specified points always takes the shortest path 
C       around the globe, meaning that a line between 0 and 360 is 
C       of length 0 in longitude. Therefore it now takes at least 
C       3 points (0., 180, 360.) and preferably 4 points (eliminating
C       all possible ambiguity) to draw a line spanning the globe in 
C       longitude. This script has been modified to work with the 
C       new rule.    
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFMapPlotClass
      external NhlFGraphicStyleClass

      integer rlist
      integer appid,wid,canvas,gsid
      integer i 
      real px(9),py(9)
      character*16 projection(4)
      data projection / 'orthographic','mollweide','stereographic',
     1 'lambertequalarea'/
  
      character*7  wks_type
C
C Define the workstation type
C
      wks_type = "x11"

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

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C  Create a meta file workstation.
C   
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName','./pr05f.ncgm',ierr)
         call NhlFCreate(wid,'pr05Work',NhlFncgmWorkstationClass,
     1        0,rlist,ierr)
    
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C  Create an X workstation.
C   
         call NhlFRLClear(rlist)
         call NhlFRLSetInteger(rlist,'wkPause','true',ierr)
         call NhlFCreate(wid,'pr05Work',
     1        NhlFCairoWindowWorkstationClass,
     1        appid,rlist,ierr)
    
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C  Create an older-style PS workstation.
C   
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPSFileName','./pr05f.ps',ierr)
         call NhlFCreate(wid,'pr05Work',NhlFpsWorkstationClass,
     1        appid,rlist,ierr)

      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C  Create an older-style PDF workstation.
C   
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPDFFileName','./pr05f.pdf',ierr)
         call NhlFCreate(wid,'pr05Work',NhlFpdfWorkstationClass,
     1        appid,rlist,ierr)

      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C  Create a cairo PS/PDF workstation.
C   
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetString(rlist,'wkFileName','./pr05f',ierr)
         call NhlFCreate(wid,'pr05Work',
     1        NhlFCairoPSpdfWorkstationClass,appid,rlist,ierr)

      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C  Create a cairo PNG workstation.
C   
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetString(rlist,'wkFileName','./pr05f',ierr)
         call NhlFCreate(wid,'pr05Work',
     1        NhlFCairoImageWorkstationClass,appid,rlist,ierr)

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

      do 10 i=1,4
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
         py(4) = 23.5
         py(5) = 23.5
         py(6) = -23.5
         py(7) = -23.5
         py(8) = -23.5
         py(9) = -23.5
         px(1) = 360.
         px(2) = 360.
         px(3) = 240.
         px(4) = 120.
         px(5) = 0.
         px(6) = 0.
         px(7) = 120.
         px(8) = 240.
         px(9) = 360.
         call NhlFDataPolygon(canvas,gsid,px,py,9,ierr)
C
C Next draw the north and south temperate zones
C Note the px array does not need to change for the 
C next four NhlDataPolygon draws
C
         call NhlFRLClear(rlist)
         call NhlFRLSetInteger(rlist,'gsFillColor',3,ierr)
         call NhlFSetValues(gsid,rlist,ierr)

         py(1) = 23.5
         py(2) = 66.5
         py(3) = 66.5
         py(4) = 66.5
         py(5) = 66.5
         py(6) = 23.5
         py(7) = 23.5
         py(8) = 23.5
         py(9) = 23.5


         call NhlFDataPolygon(canvas,gsid,px,py,9,ierr)

         py(1) = -23.5
         py(2) = -66.5
         py(3) = -66.5
         py(4) = -66.5
         py(5) = -66.5
         py(6) = -23.5
         py(7) = -23.5
         py(8) = -23.5
         py(9) = -23.5

         call NhlFDataPolygon(canvas,gsid,px,py,9,ierr)
C
C Draw the frigid zones
C
         call NhlFRLClear(rlist)
         call NhlFRLSetInteger(rlist,'gsFillColor',4,ierr)
         call NhlFSetValues(gsid,rlist,ierr)

         py(1) = 90.
         py(2) = 66.5
         py(3) = 66.5
         py(4) = 66.5
         py(5) = 66.5
         py(6) = 90.
         py(7) = 90.
         py(8) = 90.
         py(9) = 90.
         call NhlFDataPolygon(canvas,gsid,px,py,9,ierr)

         py(1) = -90.
         py(2) = -66.5
         py(3) = -66.5
         py(4) = -66.5
         py(5) = -66.5
         py(6) = -90.
         py(7) = -90.
         py(8) = -90.
         py(9) = -90.
         call NhlFDataPolygon(canvas,gsid,px,py,9,ierr)
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

         px(1) = 0
         px(2) = 120.0
         px(3) = 240.0
         px(4) = 360.0
         py(1) = 0
         py(2) = 0
         py(3) = 0
         py(4) = 0
         call NhlFDataPolyline(canvas,gsid,px,py,4,ierr)
C
C Tropic of cancer
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'gsLineLabelString',
     +        't|r|o|p|i|c o|f c|a|n|c|e|r',ierr)
         call NhlFSetValues(gsid,rlist,ierr)

         py(1) = 23.5
         py(2) = 23.5
         py(3) = 23.5
         py(4) = 23.5
         call NhlFDataPolyline(canvas,gsid,px,py,4,ierr)
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

         py(1) = -23.5
         py(2) = -23.5
         py(3) = -23.5
         py(4) = -23.5
         call NhlFDataPolyline(canvas,gsid,px,py,4,ierr)
C
C Arctic circle
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'gsLineLabelString',
     +        'a|r|c|t|i|c c|i|r|c|l|e',ierr)
         call NhlFSetValues(gsid,rlist,ierr)

         py(1) = 66.5
         py(2) = 66.5
         py(3) = 66.5
         py(4) = 66.5
         call NhlFDataPolyline(canvas,gsid,px,py,4,ierr)
C
C Antarctic circle
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'gsLineLabelString',
     +        '|a|n|t|a|r|c|t|i|c c|i|r|c|l|e',ierr)
         call NhlFSetValues(gsid,rlist,ierr)

         py(1) = -66.5
         py(2) = -66.5
         py(3) = -66.5
         py(4) = -66.5
         call NhlFDataPolyline(canvas,gsid,px,py,4,ierr)

         call NhlFFrame(wid,ierr)
 10   continue
      
      call NhlFClose
      stop
      end
