;
;      $Id: pr03n.ncl,v 1.2 1996-05-13 20:24:58 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       pr03n.ncl
;;
;;  Author:     Dave Brown
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Mon May 13 13:38:46 MDT 1996
;;
;;  Description:    Demonstrates graphics primitives drawn in NDC space
;;                  Many of the primitives are clipped in order to test
;;                  the way clipping works.
;;

;
; Begin NCL script.
;
begin

;
; Create data
;

plx = (/ 0.1,0.9,0.5,0.1 /)
ply = (/ 0.1,0.1,0.9,0.1 /)
pmx = (/ 0.05,0.95,0.5,0.5 /)
pmy = (/ 0.05,0.05,1.05,0.5 /)
pgx = (/ 0.2,0.8,0.5,0.2 /)
pgy = (/ 0.25,0.25,0.85,0.25 /)
dpgx = (/ 10.0,110.0,110.0,5.0,110.0,10.0,10.0 /)
dpgy = (/ 1.,1.,20.0,20.,110.,110,1.0 /)

;
; Create Application object.  The Application object name is used to
; determine the name of the resource file, which is "pr03.res" in this
; case.
; 
appid = create "pr03" appClass defaultapp 
    "appDefaultParent" : "True"
    "appUsrDir" : "./"
end create

;
; Default is to display output to an X workstation
;
NCGM=0
X11=1
PS=0

if (NCGM .eq. 1) then
;
; Create an ncgmWorkstation object.
;
  wid = create "pr03Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "pr03n.ncgm"
  end create
else
  if (X11 .eq. 1) then
;
; Create an XWorkstation object.
;
  wid = create "pr03Work" xWorkstationClass defaultapp
      "wkPause" : "True"
  end create
  else
    if (PS .eq. 1) then
;
; Create a PSWorkstation object.
;
      wid = create "pr03Work" psWorkstationClass defaultapp
        "wkPSFileName" : "pr03n.ps"
      end create
    end if
  end if
end if

;
; Get the workstation default GraphicStyle and change the value of
; one of its resources
;
getvalues  wid
  "wkDefGraphicStyleId"  :  id
end getvalues

setvalues id
  "gsLineColor" : 2
end setvalues

;
; Create a LogLinPlot that covers the entire NDC space 
; to use as a drawing canvas
;
canvas = create "canvas" logLinPlotClass wid
  "vpXF": 0.0
  "vpYF" : 1.0
  "vpWidthF" : 1.0
  "vpHeightF" : 1.0
end create

;
; Explicitly create a GraphicStyle
; 
gsid = create "style" graphicStyleClass wid end create

;
; Set workstation line attributes and draw a triangle
;
setvalues gsid
  "gsLineDashPattern" : 2
  "gsLineThicknessF" : 3.0
end setvalues

draw(canvas)

;
; The polyline call uses the default GraphicStyle; Polygon and
; Polymarker use the explicitly created GraphicStyle.
;
NhlNDCPolyline(canvas,id,plx,ply)
NhlNDCPolygon(canvas,gsid,pgx,pgy)
NhlNDCPolymarker(canvas,gsid,pmx,pmy)
frame(wid)

setvalues canvas
  "vpXF" : 0.2
  "vpYF" : 0.8
  "vpWidthF" : 0.6
  "vpHeightF" : 0.6
end setvalues

draw(canvas)
NhlNDCPolyline(canvas,gsid,plx,ply)
NhlNDCPolygon(canvas,gsid,pgx,pgy)
NhlNDCPolymarker(canvas,gsid,pmx,pmy)
frame(wid)

plx = plx + 0.4
ply = ply + 0.4
pgx = pgx + 0.4
pgy = pgy + 0.4
pmx = pmx + 0.4
pmy = pmy + 0.4
       
draw(canvas)
NhlNDCPolyline(canvas,gsid,plx,ply)
NhlNDCPolygon(canvas,gsid,pgx,pgy)
NhlNDCPolymarker(canvas,gsid,pmx,pmy)
frame(wid)

plx = plx - 0.4
ply = ply - 0.4
pgx = pgx - 0.4
pgy = pgy - 0.4
pmx = pmx - 0.4
pmy = pmy - 0.4
       
;
; Modify the data values to be in a meaningful range relative to the
; data coordinate extent.
;
plx = plx * 100.0 + 50.
ply = ply * 100.0 + 50.
pgx = pgx * 100.0 + 50.
pgy = pgy * 100.0 + 50.
pmx = pmx * 100.0 + 50.
pmy = pmy * 100.0 + 50.

draw(canvas)
NhlDataPolyline(canvas,gsid,plx,ply)
NhlDataPolymarker(canvas,gsid,pmx,pmy)
NhlDataPolygon(canvas,gsid,pgx,pgy)
frame(wid)

plx = plx - 80.
ply = ply - 80.
pgx = pgx - 80.
pgy = pgy - 80.
pmx = pmx - 80.
pmy = pmy - 80.


draw(canvas)
NhlDataPolyline(canvas,gsid,plx,ply)
NhlDataPolymarker(canvas,gsid,pmx,pmy)
NhlDataPolygon(canvas,gsid,pgx,pgy)
frame(wid)

draw(canvas)
NhlDataPolygon(canvas,gsid,dpgx,dpgy)
frame(wid)

;
; End NCL script.
;
end
