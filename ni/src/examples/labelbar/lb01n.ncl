
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1995                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      File:           lb01n.ncl
;
;      Author:         Bob Lackman
;          National Center for Atmospheric Research
;          PO 3000, Boulder, Colorado
;
;      Date:           Fri Jan 13 18:31:18 MDT 1995
;
;      Description:    Demonstrates the LabelBar Object defaults.
;

;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; In this example the resource file supplies the plot title only.
;
appid = create "lb01" appLayerClass defaultapp
	"appDefaultParent" : "True"
	"appUsrDir" : "./"
end create

;
; Create an XWorkstation object.
;
wid = create "lb01Work" xWorkstationLayerClass defaultapp
	"wkPause" : "True"
end create

;
; Specify the viewport extent of the object.
;
pid = create "LabelBar" labelBarLayerClass wid
	"vpXF" : 0.
	"vpYF" : 1.
	"vpWidthF" : 1.
	"vpHeightF" : 1.
end create

draw(pid)
frame(wid)
