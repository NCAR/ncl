
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;/
;
;      File:           lg01n.ncl
;
;      Author:         Ed Stautler
;			National Center for Atmospheric Research
;			PO 3000, Boulder, Colorado
;
;      Date:           Fri Jan 13 18:31:18 MDT 1995
;
;      Description:    Demonstrates the Legend Object defaults.
;


;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; In this example the resource file supplies the plot title only.
;
appid = create "lg01" appLayerClass noparent
	"appUsrDir" : "./"
end create

;
; Create an XWorkstation object.
;
wid = create "lg01Work" xWorkstationLayerClass noparent
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
end create

draw(pid);
frame(wid);
delete(pid);
delete(wid);
delete(appid);
