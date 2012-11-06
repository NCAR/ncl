;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1993                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      File:           lb02n.ncl
;
;      Author:         Bob Lackman
;          National Center for Atmospheric Research
;          PO 3000, Boulder, Colorado
;
;      Date:           Fri Jan 13 18:31:18 MDT 1995
;
;      Description:    Demonstrates the LabelBar Object
;                      Creates color bars with every 5th index of the
;                      255 different colors in the default colormap.
;

;
; begin NCL script
; 
begin

;
; Initialize data values
;
colors = ispan(1,255,5)

line_labels = "Color Index " + colors

;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; In this example the resource file supplies the plot title only.
;
appid = create "lb02" appClass defaultapp
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
  wid = create "lb02Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./lb02n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X Workstation object.
;
  wid = create "lb02Work" windowWorkstationClass defaultapp
    "wkPause" : True
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "lb02Work" psWorkstationClass defaultapp
    "wkPSFileName" : "./lb02n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "lb02Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "./lb02n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF workstation.
;
  wid = create "lb02Work" documentWorkstationClass defaultapp
    "wkFileName" : "./lb02n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG workstation.
;
  wid = create "lb02Work" imageWorkstationClass defaultapp
    "wkFileName" : "./lb02n"
    "wkFormat" : wks_type
  end create
end if

;
; Create a plot with 51 color indices (Every fifth of the default
; workstation colormap.
;
pid = create "LabelBar" labelBarClass wid
    "lbBoxCount"     : dimsizes(colors)
    "lbFillColors" : colors
    "lbLabelStrings" : line_labels
    "vpXF" : 0.
    "vpYF" : 1.
    "vpWidthF" : 1.
    "vpHeightF" : 1.
end create

draw(pid);
frame(wid);

;
; end NCL script
; 
end

