;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1996                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       pr01n.ncl
;
;   Author: Lynn Hermanson
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
; 
;   Date:       Apr 29, 1996
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

X(0) = .3
X(1) = .3
X(2) = .5
X(3) = .5
X(4) = .3
Y(0) = .3
Y(1) = .5
Y(2) = .5
Y(3) = .3
Y(4) = .3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;These are the polymarker points /*four corner points*/

M = 4

U = new((/M/),float)
V = new((/M/),float)

U(0) = .4
U(1) = .4
U(2) = .6
U(3) = .6
V(0) = .4
V(1) = .6
V(2) = .6
V(3) = .4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;These are the polygon points /*a filled rectangle*/

K=5

PX = new((/K/),float)
PY = new((/K/),float)

PX(0) = .7
PX(1) = .7
PX(2) = .8
PX(3) = .8
PX(4) = .7
PY(0) = .2
PY(1) = .8
PY(2) = .8
PY(3) = .2
PY(4) = .2



; Next,
; create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; In the following examples default resources are used. 
;
appid = create "pr01" appClass defaultapp
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
  wid = create "pr01Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./pr01n.ncgm"
  end create
else
  if (X11.eq.1) then
;
; Create an X workstation.
;
    wid = create "pr01Work" xWorkstationClass defaultapp
      "wkPause" : "True"
    end create
  else
    if (PS .eq. 1) then
;
; Create a PS workstation.
;
      wid = create "pr01Work" psWorkstationClass defaultapp
        "wkPSFileName" : "./pr01n.ps"
      end create
    end if
  end if
end if

; Create a blank plot object to place the primitives on.
cid = create "canvas" logLinPlotClass wid
end create

; Create a graphicPlot object consisting of, the default styles.
; The graphicStyle object is NOT a stand-alone plot object. There
; must exist some kind of plot object which has already been drawn
; to place the primitives on.

gsid = create "defaultstyle" graphicStyleClass wid
end create

; Draw the shapes onto the blank logLinPlot object.
; Signify end of frame.

draw(cid)
NhlNDCPolyline(cid, gsid, X, Y)
NhlNDCPolymarker(cid, gsid, U, V)
NhlNDCPolygon(cid, gsid, PX, PY)
frame(wid)

; Destroy all of the objects created, close the HLU library and exit.

delete(cid)
delete(wid)
delete(appid)

end
