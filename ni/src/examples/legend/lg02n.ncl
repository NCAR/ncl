
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
labels = (/"Marker_0","Marker_1","Marker_2","Marker_3","Marker_4" /)
colors = (/ 11, 21, 31, 41, 51 /)
mkthik = (/ 2.0, 3.0, 4.0, 5.0, 6.0 /)
markrs = (/ 1, 1, 1, 1, 1 /)
item_ind = (/ 2, 3, 4, 5, 6 /)

;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; In this example the resource file supplies the plot title only.
;
appid = create "lg02" appLayerClass noparent
        "appUsrDir" : "./"
end create

;
; Create an XWorkstation object.
;
wid = create "lg02Work" xWorkstationLayerClass noparent
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
	"lgItemTypes" : markrs
	"lgItemColors" : colors
	"lgItemIndexes" : item_ind
	"lgMonoItemThickness" : "False"
	"lgItemThicknesses" : mkthik
	"lgItemFontHeightF" : .05
end create

draw(pid)
frame(wid)
delete(pid)
delete(wid)
delete(appid)
