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
;                      131 different colors in the default colormap.
;

;
; begin NCL script
; 
begin

;
; Initialize data values
;
colors = (/ 1,7,13,19,25,31,37,43,49,55,61,67,73,79,85,91,97, \
            103,109,115,121,127/)

line_labels = (/"Color Index 1 ","Color Index 7 ","Color Index 13", \
            "Color Index 19","Color Index 25","Color Index 31", \
            "Color Index 37","Color Index 43","Color Index 49", \
            "Color Index 55","Color Index 61","Color Index 67", \
            "Color Index 73","Color Index 79","Color Index 85", \
            "Color Index 91","Color Index 97","Color Index 103", \
            "Color Index 109","Color Index 115","Color Index 121",\
            "Color Index 127"/)
;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; In this example the resource file supplies the plot title only.
;
appid = create "lb02" appClass defaultapp
    "appDefaultParent" : "True"
    "appUsrDir" : "./"
end create

;
; Default is to display output to an X workstation
;
NCGM=0

if (NCGM.eq.1) then
;
; Create an NCGM workstation.
;
  wid = create "lb02Work" ncgmWorkstationClass appid
    "wkMetaName" : "./lb02n.ncgm"
  end create
else
;
; Create an X Workstation object.
;
  wid = create "lb02Work" xWorkstationClass defaultapp
    "wkPause" : "True"
  end create
end if

;
; Create a plot with 22 color indices (Every 6th one of the default
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

