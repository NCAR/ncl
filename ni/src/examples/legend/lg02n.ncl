;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;/
;
;      File:           lg02n.ncl
;
;      Author:         Bob Lackman (converted by Ed Stautler)
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;      Date:           Fri Jan 13 18:31:18 MDT 1995
;
;      Description:    Demonstrates a Legend of 5 markers.
;

;
; Begin NCL script.
;
begin

;
; Initialize data values
;
labels = (/"Marker_0","Marker_1","Marker_2","Marker_3","Marker_4" /)
colors = (/ 3, 5, 7, 9, 11 /)
mkthik = (/ 2.0, 3.0, 4.0, 5.0, 6.0 /)
item_ind = (/ 2, 3, 4, 5, 6 /)

;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; In this example the resource file supplies the plot title only.
;
appid = create "lg02" appClass defaultapp
        "appDefaultParent" : True
        "appUsrDir" : "./"
end create

;
; Default is to display output to an X workstation
;
NCGM=0
X11=1
PS=0

if (NCGM.eq.1) then
;
; Create an NCGM workstation.
;
  wid = create "lg02Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./lg02n.ncgm"
  end create
else
  if (X11.eq.1) then
;
; Create an X Workstation.
;
    wid = create "lg02Work" xWorkstationClass defaultapp
      "wkPause" : True
    end create
  else
    if (PS .eq. 1) then
;
; Create a PS workstation.
;
      wid = create "lg02Work" psWorkstationClass defaultapp
        "wkPSFileName" : "./lg02n.ps"
      end create
    end if
  end if
end if

;
; Specify the viewport extent of the object.
;
pid = create "Legend" legendClass wid
        "vpXF" : 0.0
        "vpYF" : 1.0
        "vpWidthF" : 1.0
        "vpHeightF" : 1.0
;
; Specify the type of markers for the legend.
;
    "lgItemCount" : 5
    "lgMonoItemType" : True
    "lgItemType" : "MARKERS"
    "lgLabelStrings" : labels
    "lgMarkerColors" : colors
    "lgMarkerIndexes" : item_ind
    "lgMonoMarkerThickness" : False
    "lgMarkerThicknesses" : mkthik
    "lgMarkerSizeF" : 0.05
end create

draw(pid)
frame(wid)
delete(pid)
delete(wid)
delete(appid)

;
; End NCL script.
;
end
