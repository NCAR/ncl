
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;/
;
;      File:           lg03c.c
;
;      Author:         Ed Stautler
;			National Center for Atmospheric Research
;			PO 3000, Boulder, Colorado
;
;      Date:           Fri Jan 13 18:31:18 MDT 1995
;
;      Description:    Demonstrates a Legend of 5 markers.
;

;
; Initialize data values
;
labels = (/"Line_Type_0","Line_Type_1","Line_Type_2", \
           "Line_Type_3","Line_Type_4" /)
colors = (/ 40, 57, 65, 80, 90 /)
lnthik = (/ 4.0, 4.0, 4.0, 4.0, 4.0 /)
types = (/ 0, 0, 0, 0, 0 /)
item_ind = (/ 2, 3, 4, 5, 6 /)

;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; In this example the resource file supplies the plot title only.
;
appid = create "lg03" appLayerClass noparent
        "appUsrDir" : "./"
end create

;
; Create an XWorkstation object.
;
wid = create "lg03Work" xWorkstationLayerClass noparent
        "wkPause" : "True"
end create

;
; Specify the viewport extent of the object.
;
pid = create "Legend" legendLayerClass wid
        "vpXF" : 0.0
        "vpYF" : 1.0
        "vpWidthF" : 1.0
        "vpHeightF" : 1.0
;
; Specify the type of markers for the legend.
;
	"lgItemCount" : 5
	"lgMonoItemType" : "False"
	"lgLabelFontHeightF" : .03
	"lgLabelStrings" : labels
	"lgItemTypes" : types
;
; Set the dashed lines and the line characters to the same colors.
;
	"lgItemColors" : colors
        "lgItemStringColors" : colors
	"lgItemIndexes" : item_ind
	"lgMonoItemThickness" : "False"
	"lgItemThicknesses" : lnthik
	"lgItemFontHeightF" : .03
end create

draw(pid)
frame(wid)
delete(pid)
delete(wid)
delete(appid)
