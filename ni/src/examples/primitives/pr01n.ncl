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



X =(/ .3 , .3 , .5 , .5 , .3/)
Y =(/ .3 , .5 , .5 , .3 , .3/)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;These are the polymarker points /*four corner points*/


U = (/.4 , .4 , .6 , .6/)
V = (/.4 , .6 , .6 , .4/)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;These are the polygon points /*a filled rectangle*/



PX = (/ .7 , .7 , .8 , .8 , .7 /)
PY = (/ .2 , .8 , .8 , .2 , .2 /)



; Next,
; create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; In the following examples default resources are used. 
;
appid = create "pr01" appClass defaultapp
    "appUsrDir" : "./"
        "appDefaultParent" : True
end create

; Choose to display output to an X11 workstation. 

wks_type = "x11"

if (str_lower(wks_type).eq."ncgm")
;
; Create an NCGM workstation.
;
  wid = create "pr01Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./pr01n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X workstation.
;
  wid = create "pr01Work" windowWorkstationClass defaultapp
    "wkPause" : True
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "pr01Work" psWorkstationClass defaultapp
    "wkPSFileName" : "./pr01n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "pr01Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "./pr01n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wid = create "pr01Work" documentWorkstationClass defaultapp
    "wkFileName" : "./pr01n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wid = create "pr01Work" imageWorkstationClass defaultapp
    "wkFileName" : "./pr01n"
    "wkFormat" : wks_type
  end create
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
