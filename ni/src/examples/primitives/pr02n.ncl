;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1996                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       pr02n.ncl
;
;   Author: Lynn Hermanson
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
; 
;   Date:       May 8, 1996
;
;   Description:     Given an array of xaxis values and 
;                    an array of yaxis values, where each 
;                    x,y pair corresponds to one vertex or
;                    the location of one point, draw a
;                    square (a polyline), four points (asterisks)
;                    at the corners of a square (a polymarker), and
;                    a rectangular filled area (a polygon),on to
;                    a blank canvas (an empty default logLinPlot).
;
;                    Polylines, polymarkers, and polygons, are
;                    refered to as primitives.

begin

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;These are the polyline points  /*a square*/

N=5

X = new((/N/),float)
Y = new((/N/),float)

X(0) = .25
X(1) = .25
X(2) = .45
X(3) = .45
X(4) = .25
Y(0) = .25
Y(1) = .45
Y(2) = .45
Y(3) = .25
Y(4) = .25

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;These are the polymarker points /*four corner points*/

M = 4

U = new((/M/),float)
V = new((/M/),float)

U(0) = .35
U(1) = .35
U(2) = .55
U(3) = .55
V(0) = .35
V(1) = .55
V(2) = .55
V(3) = .35


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;These are the polygon points /*a filled rectangle*/

K=5

PX = new((/K/),float)
PY = new((/K/),float)

PX(0) = .6
PX(1) = .7
PX(2) = .7
PX(3) = .8
PX(4) = .6
PY(0) = .2
PY(1) = .8
PY(2) = .8
PY(3) = .2
PY(4) = .2




; Next,
; create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; ***See resource file pr02.res*** 
;
appid = create "pr02" appClass defaultapp
    "appUsrDir" : "./"
        "appDefaultParent" : "True"
end create

; Choose to display output to an X11 workstation. 

NCGM=0
X11=1
PS=0

if (NCGM.eq.1) then
;
; Create an NCGM workstation.
;
  wid = create "pr02Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./pr02n.ncgm"
  end create
else
  if (X11.eq.1) then
;
; Create an X workstation.
;
    wid = create "pr02Work" xWorkstationClass defaultapp
      "wkPause" : "True"
    end create
  else
    if (PS .eq. 1) then
;
; Create a PS workstation.
;
      wid = create "pr02Work" psWorkstationClass defaultapp
        "wkPSFileName" : "./pr02n.ps"
      end create
    end if
  end if
end if

; Create a blank plot object to place the primitives on.
cid = create "canvas" logLinPlotClass wid
end create

; Create a graphicPlot object consisting of a thick blue square, large
; red filled circular corner points,and a dashed red triangle 
; filled with green stripes.
; The graphicStyle object is NOT a stand-alone plot object. There
; must exist some kind of plot object which has already been drawn
; to place the primitives on.

gsid_pl = create "pr02polyline" graphicStyleClass wid

;;;**** See resource file pr02.res ****
;;;        "gsLineColor" : 4
;;;        "gsLineThicknessF" : 3

end create

gsid_pm = create "pr02polymarker" graphicStyleClass wid

;;;**** See resource file pr02.res ****
;;;        "gsMarkerIndex" : 16
;;;        "gsMarkerColor" : 2
;;;        "gsMarkerSizeF" : .02

end create

gsid_pg = create "pr02polygon" graphicStyleClass wid

;;;**** See resource file pr02.res ****
;;;        "gsEdgesOn" : "true"
;;;        "gsEdgeDashPattern" : 1
;;;        "gsEdgeColor" : 2
;;;        "gsEdgeThicknessF" : 3
;;;        "gsFillColor" : 3
;;;        "gsFillIndex" : 1

end create


; Draw the shapes onto the blank logLinPlot object.
; Signify end of frame.

draw(cid)
NhlNDCPolyline(cid, gsid_pl, X, Y)
NhlNDCPolymarker(cid, gsid_pm, U, V)
NhlNDCPolygon(cid, gsid_pg, PX, PY)
frame(wid)

; Destroy all of the objects created, close the HLU library and exit.

delete(cid)
delete(wid)
delete(appid)

end
