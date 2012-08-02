;
;      $Id: pr05n.ncl,v 1.5 2010-03-15 22:49:24 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1996                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  File:       pr05n.ncl
;
;  Author:     Dave Brown (converted by Mary Haley)
;          National Center for Atmospheric Research
;          PO 3000, Boulder, Colorado
;
;  Date:       Mon Jul  1 09:41:40 MDT 1996
;
;  Description:    Demonstrates graphics primitives drawn in data
;                  space over a MapPlot. Data polygons are used to
;                  color global zones and data polylines mark the
;                  zonal boundaries. Line labels name the boundary
;                  lines (e.g. arctic circle). Markers are drawn at
;                  the poles. The three frames present three 
;                  different map projections, showing how the 
;                  primitives adapt to various levels of distortion.
;

;
; Begin NCL script.
;
begin

;
; Initialize variables
;
projection = (/"orthographic","mollweide","stereographic"/)

;
; Create Application object.  The Application object name is used to
; determine the name of the resource file, which is "pr05.res" in this
; case.
; 
appid = create "pr05" appClass defaultapp 
    "appDefaultParent" : True
    "appUsrDir" : "./"
end create

;
; Default is to display output to an X workstation
;
wks_type = "x11"

if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
  wid = create "pr05Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "pr05n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
wid = create "pr05Work" windowWorkstationClass defaultapp
    "wkPause" : True
end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "pr05Work" psWorkstationClass defaultapp
    "wkPSFileName" : "pr05n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "pr05Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "pr05n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wid = create "pr05Work" documentWorkstationClass defaultapp
    "wkFileName" : "pr05n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wid = create "pr05Work" imageWorkstationClass defaultapp
    "wkFileName" : "pr05n"
    "wkFormat" : wks_type
  end create
end if

;
; Create a MapPlot that covers the entire NDC space 
; to use as a drawing canvas
;
canvas = create "canvas" mapPlotClass wid
  "vpXF" : 0.0
  "vpYF" : 1.0
  "vpWidthF" : 1.0
  "vpHeightF" : 1.0
end create

;
; Create a GraphicStyle to control the primitive attributes.
;
gsid = create "style" graphicStyleClass wid end create

do i = 0,2

;
; Set the map projection
;
  setvalues canvas
    "mpProjection" : projection(i)
  end setvalues
;
; Draw polygons representing the 5 major zones of the globe, beginning
; with the tropical zone.
; Turn edges off and set the marker color.
;
  setvalues gsid
    "gsMarkerColor" : 0
    "gsEdgesOn" : False
    "gsFillColor" : 2
  end setvalues

  px = (/360.,360.,0.,0.,360./)
  py = (/-23.5,23.5,23.5,-23.5,-23.5/)
  NhlDataPolygon(canvas,gsid,px,py)

;
; Next draw the north and south temperate zones
;
  setvalues gsid
    "gsFillColor" : 3
  end setvalues

  py(0:4) = (/ 23.5 , 66.5 , 66.5 , 23.5 , 23.5/)
  px(0:4) = (/ 360. , 360. , 0. , 0. , 360./)
  NhlDataPolygon(canvas,gsid,px,py)

  py(0:4) = (/ -23.5 , -66.5 , -66.5 , -23.5 , -23.5 /)
  px(0:4) = (/ 360. , 360. ,  0. , 0.  , 360. /)
  NhlDataPolygon(canvas,gsid,px,py)
;
; Draw the frigid zones
;
  setvalues gsid
    "gsFillColor" : 4
  end setvalues

  py(0:4) = (/ 90. , 66.5 , 66.5 , 90. , 90. /)
  px(0:4) = (/ 360.  , 360. , 0. , 0. , 360. /)
  NhlDataPolygon(canvas,gsid,px,py)

  py(0:4) = (/ -90. , -66.5 , -66.5 , -90. , -90./)
  px(0:4) = (/ 360. , 360.  , 0. , 0. , 360./)
  NhlDataPolygon(canvas,gsid,px,py)

;
; Draw the map outlines and grid
;
  draw(canvas)
;
; Draw markers at each pole
;
  px(0:1) = 0.
  py(0:1) = (/90., -90./)
  NhlDataPolymarker(canvas,gsid,px(:1),py(:1))

;
; Draw polylines at each of the major latitudinal boundary lines,
; beginning with the equator. Use the line label to name each of the
; lines. The '|' character is inserted between each label character 
; to allow the labels to track the curve of each line more precisely.
;
  setvalues gsid
    "gsLineLabelString" : "e|q|u|a|t|o|r"
  end setvalues

  px(0:1) = (/360.,0/)
  py(0:1) = 0.
  NhlDataPolyline(canvas,gsid,px(:1),py(:1))

;
; Tropic of cancer
;
  setvalues gsid
    "gsLineLabelString" : "t|r|o|p|i|c o|f c|a|n|c|e|r"
  end setvalues

  px(0:1) = (/360., 0./)
  py(0:1) = 23.5
  NhlDataPolyline(canvas,gsid,px(:1),py(:1))

;
; Tropic of capricorn (Note: currently there is a limit on the 
; number of characters in a line label that prevents the '|'
; character from being used between each letter in a label 
; of this length).
;
  setvalues gsid
    "gsLineLabelString" : "tr|o|p|ic of c|a|p|r|i|c|o|rn"
  end setvalues

  py(0:1) = -23.5
  NhlDataPolyline(canvas,gsid,px(:1),py(:1))
;
; Arctic circle
;
  setvalues gsid
    "gsLineLabelString" : "a|r|c|t|i|c c|i|r|c|l|e"
  end setvalues

  py(0:1) = 66.5
  NhlDataPolyline(canvas,gsid,px(:1),py(:1))

;
; Antarctic circle
;
  setvalues gsid
    "gsLineLabelString" : "|a|n|t|a|r|c|t|i|c c|i|r|c|l|e"
  end setvalues

  py(0:1) = -66.5
  NhlDataPolyline(canvas,gsid,px(:1),py(:1))
  frame(wid)

end do

;
; End NCL script.
;
end
