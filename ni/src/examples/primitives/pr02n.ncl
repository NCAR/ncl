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

X = (/ .25 , .25 , .45 , .45 , .25 /)
Y = (/ .25 , .45 , .45 , .25 , .25 /)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;These are the polymarker points /*four corner points*/

U = (/ .35 , .35 , .55 , .55 /)
V = (/ .35 , .55 , .55 , .35 /)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;These are the polygon points /*a filled rectangle*/



PX = (/ .6 , .7 , .7 , .8 , .6 /)
PY = (/ .2 , .8 , .8 , .2 , .2 /)




; Next,
; create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; ***See resource file pr02.res*** 
;
appid = create "pr02" appClass defaultapp
    "appUsrDir" : "./"
        "appDefaultParent" : True
end create

; Choose to display output to an X11 workstation. 

wks_type = "x11"

if (str_lower(wks_type).eq."ncgm")
;
; Create an NCGM workstation.
;
  wid = create "pr02Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./pr02n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X workstation.
;
  wid = create "pr02Work" windowWorkstationClass defaultapp
    "wkPause" : True
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "pr02Work" psWorkstationClass defaultapp
    "wkPSFileName" : "./pr02n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "pr02Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "./pr02n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wid = create "pr02Work" documentWorkstationClass defaultapp
    "wkFileName" : "./pr02n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wid = create "pr02Work" imageWorkstationClass defaultapp
    "wkFileName" : "./pr02n"
    "wkFormat" : wks_type
  end create
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
;;;        "gsEdgesOn" : True
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
