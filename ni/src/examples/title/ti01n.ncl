;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      File:           ti01n.ncl
;
;      Author:         Bob Lackman (converted by Ed Stautler)
;          National Center for Atmospheric Research
;          PO 3000, Boulder, Colorado
;
;      Date:           Fri Jan 06 18:31:18 MDT 1995
;
;      Description:    Demonstrates the Title Object
;                      defaults.
;


;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; In this example the resource file supplies the plot title only.
;
appid = create "ti01" appLayerClass defaultapp
    "appDefaultParent" : "True"
    "appUsrDir" : "./"
end create

;
; Create an XWorkstation object.
;
wid = create "ti01Work" xWorkstationLayerClass defaultapp
    "wkPause" : "True"
end create

;
; Specify the viewport extent of the object.
;
pid = create "Titles" titleLayerClass wid
    "vpXF" : .2
    "vpYF" : .8
    "vpWidthF" : .6
    "vpHeightF" : .6
end create

draw(pid);
frame(wid);

delete(pid);
delete(wid);
delete(appid);
