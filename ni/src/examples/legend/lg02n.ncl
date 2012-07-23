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
wks_type = "x11"

if (str_lower(wks_type).eq."ncgm")
;
; Create an NCGM workstation.
;
  wid = create "lg02Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./lg02n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X Workstation.
;
  wid = create "lg02Work" windowWorkstationClass defaultapp
    "wkPause" : True
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "lg02Work" psWorkstationClass defaultapp
    "wkPSFileName" : "./lg02n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "lg02Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "./lg02n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF workstation.
;
  wid = create "lg02Work" documentWorkstationClass defaultapp
    "wkFileName" : "./lg02n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG workstation.
;
  wid = create "lg02Work" imageWorkstationClass defaultapp
    "wkFileName" : "./lg02n"
    "wkFormat" : wks_type
  end create
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
