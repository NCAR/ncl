;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1995                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;                      114 different colors in the default colormap.
;

;
; Initialize data values
;
colors = (/ 1,6,11,16,21,26,31,36,41,46,51,56, \
            61,66,71,76,81,86,91,96,101,106    /)

line_labels = (/"Color Index 1 ","Color Index 6 ","Color Index 11", \
            "Color Index 16","Color Index 21","Color Index 26", \
            "Color Index 31","Color Index 36","Color Index 41", \
            "Color Index 46","Color Index 51","Color Index 56", \
            "Color Index 61","Color Index 66","Color Index 71", \
            "Color Index 76","Color Index 81","Color Index 86", \
            "Color Index 91","Color Index 96","Color Index 101",\
            "Color Index 106"/)
;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; In this example the resource file supplies the plot title only.
;
appid = create "lb02" appLayerClass defaultapp
    "appDefaultParent" : "True"
    "appUsrDir" : "./"
end create

;
; Create an XWorkstation object.
;
wid = create "lb02Work" xWorkstationLayerClass defaultapp
    "wkPause" : "True"
end create

;
; Create a plot with 22 color indices (Every 5th one of the default
; workstation colormap.
;
pid = create "LabelBar" labelBarLayerClass wid
    "lbFillColors" : colors
    "lbLabelStrings" : line_labels
    "vpXF" : 0.
    "vpYF" : 1.
    "vpWidthF" : 1.
    "vpHeightF" : 1.
end create

draw(pid);
frame(wid);
