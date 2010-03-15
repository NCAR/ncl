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
;                      Creates color bars with every 6th index of the
;                      32 different colors in the default colormap.
;

;
; begin NCL script
; 
begin

;
; Initialize data values
;
colors = (/ 1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31/)

line_labels = (/"Color Index 1 ","Color Index 3 ", \
     "Color Index 5  ","Color Index 7  ", \
     "Color Index 9  ","Color Index 11 ", \
     "Color Index 13 ","Color Index 15 ", \
     "Color Index 17 ","Color Index 19 ", \
     "Color Index 21 ","Color Index 23 ", \
     "Color Index 25 ","Color Index 27 ", \
     "Color Index 29 ","Color Index 31 "/)

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
  wid = create "lb02Work" xWorkstationClass defaultapp
    "wkPause" : True
  end create
end if
if (str_lower(wks_type).eq."ps") then
;
; Create a PS workstation.
;
  wid = create "lb02Work" psWorkstationClass defaultapp
    "wkPSFileName" : "./lb02n.ps"
  end create
end if
if (str_lower(wks_type).eq."pdf") then
;
; Create a PDF workstation.
;
  wid = create "lb02Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "./lb02n.pdf"
  end create
end if
if (str_lower(wks_type).eq."newpdf".or.str_lower(wks_type).eq."newps") then
;
; Create a cairo PS/PDF workstation.
;
  wid = create "lb02Work" documentWorkstationClass defaultapp
    "wkFileName" : "./lb02n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."newpng".or.str_lower(wks_type).eq."png") then
;
; Create a cairo PNG workstation.
;
  wid = create "lb02Work" imageWorkstationClass defaultapp
    "wkFileName" : "./lb02n"
    "wkFormat" : wks_type
  end create
end if

;
; Create a plot with 16 color indices (Every other one of the default
; workstation colormap.
;
pid = create "LabelBar" labelBarClass wid
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

