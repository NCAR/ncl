;
;      $Id: pr04n.ncl,v 1.7 2010-03-15 22:49:24 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1996                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  File:       pr04n.ncl
;
;  Author:     Dave Brown (converted by Mary Haley)
;          National Center for Atmospheric Research
;          PO 3000, Boulder, Colorado
;
;  Date:       Mon Jul  1 08:40:46 MDT 1996
;
;  Description:    Demonstrates graphics primitives drawn in NDC and 
;                  and data space into an IrregularPlot (and therefore 
;                  using the IrregularTransformation).

;
; Begin NCL script.
;
begin

;
; Create data
;

plx = (/ 0.1,0.9,0.5,0.1/)
ply = (/ 0.1,0.1,0.9,0.1/)
plx2 = (/10.,50.,90.,50.,10./)
ply2 = (/50.,10.,50.,90.,50./)
pmx = (/ 0.05,0.95,0.5,0.5 /)
pmy = (/ 0.05,0.05,1.05,0.5 /)
pgx = (/ 0.2,0.8,0.5,0.2 /)
pgy = (/ 0.25,0.25,0.85,0.25 /)
dpgx = (/ 5.0,110.0,110.0,0.0,110.0,5.0,5.0 /)
dpgy = (/ 10.,10.,20.0,20.,110.,110,10.0 /)

;
; Create Application object.  The Application object name is used to
; determine the name of the resource file, which is "pr04.res" in this
; case.
; 
appid = create "pr04" appClass defaultapp 
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
  wid = create "pr04Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "pr04n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
wid = create "pr04Work" windowWorkstationClass defaultapp
    "wkPause" : True
end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "pr04Work" psWorkstationClass defaultapp
    "wkPSFileName" : "pr04n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "pr04Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "pr04n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wid = create "pr04Work" documentWorkstationClass defaultapp
    "wkFileName" : "pr04n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wid = create "pr04Work" imageWorkstationClass defaultapp
    "wkFileName" : "pr04n"
    "wkFormat" : wks_type
  end create
end if

;
; Create an IrregularPlot that covers the entire NDC space 
; to use as a drawing canvas
;
canvas = create "canvas" irregularPlotClass wid
  "vpXF": 0.1
  "vpYF" : 0.9
  "vpWidthF" : 0.8
  "vpHeightF" : 0.8
  "pmTitleDisplayMode" : "always"
  "tiMainString" : "Irregular Plot with NDC Primitives"
end create
draw(canvas)

;
; Get the workstation default GraphicStyle.
;
getvalues  wid
  "wkDefGraphicStyleId"  :  gsid
end getvalues

NhlNDCPolyline(canvas,gsid,plx,ply)
NhlNDCPolygon(canvas,gsid,pgx,pgy)
NhlNDCPolymarker(canvas,gsid,pmx,pmy)
frame(wid)

plx = plx * 100.0 + 50.
ply = ply * 100.0 + 50.
pgx = pgx * 100.0 + 50.
pgy = pgy * 100.0 + 50.
pmx = pmx * 100.0 + 50.
pmy = pmy * 100.0 + 50.

setvalues canvas
  "tiMainString" : "Clipped Data Space Primitives"
end setvalues
draw(canvas)

NhlDataPolyline(canvas,gsid,plx,ply)
NhlDataPolymarker(canvas,gsid,pmx,pmy)
NhlDataPolygon(canvas,gsid,pgx,pgy)
frame(wid)

plx = plx - 40.
ply = ply - 40.
pgx = pgx - 40.
pgy = pgy - 40.
pmx = pmx - 40.
pmy = pmy - 40.

setvalues canvas
  "tiMainString" : "Data Space Primitives Repositioned"
end setvalues
draw(canvas)

NhlDataPolyline(canvas,gsid,plx,ply)
NhlDataPolymarker(canvas,gsid,pmx,pmy)
NhlDataPolygon(canvas,gsid,pgx,pgy)
frame(wid)

setvalues canvas
  "tiMainString" : "A Diamond in Data Space"
end setvalues
draw(canvas)

NhlDataPolygon(canvas,gsid,plx2,ply2)
plx2(0) = 5.0
plx2(2) = 95.0
plx2(4) = 5.0
ply2(1) = 5.0
ply2(3) = 95.0
NhlDataPolyline(canvas,gsid,plx2,ply2)
frame(wid)

setvalues canvas
  "tiMainString" : "A Self-Intersecting Data Polygon"
end setvalues
draw(canvas)

NhlDataPolygon(canvas,gsid,dpgx,dpgy)
frame(wid)

;
; End NCL script.
;
end
