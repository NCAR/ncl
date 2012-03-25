;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      File:           tm01n.ncl
;
;      Author:         Bob Lackman (converted by Ed Stautler)
;          National Center for Atmospheric Research
;          PO 3000, Boulder, Colorado
;
;      Date:           Fri Jan 06 18:31:18 MDT 1995
;
;      Description:    Demonstrates the TickMark Object
;                      defaults.
;

;
; Begin NCL script.
;
begin

;
; Create an application context. Set the app dir to the current
; directory so the application looks for a resource file in the working
; directory. In this example the resource file supplies the plot title
; only.
;
appid = create "tm01" appClass defaultapp
    "appUsrDir" : "./"
    "appDefaultParent" : True
end create

;
; Default is to display output to an X workstation.
;
wks_type = "x11"

if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
  wid = create "tm01Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "tm01n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  wid = create "tm01Work" windowWorkstationClass defaultapp
    "wkPause" : True
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an PS object.
;
  wid = create "tm01Work" psWorkstationClass defaultapp
    "wkPSFileName" : "tm01n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an PDF object.
;
  wid = create "tm01Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "tm01n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wid = create "tm01Work" documentWorkstationClass defaultapp
    "wkFileName" : "tm01n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wid = create "tm01Work" imageWorkstationClass defaultapp
    "wkFileName" : "tm01n"
    "wkFormat" : wks_type
  end create
end if

;
; Specify the viewport extent of the object.
;
pid = create "TickMarks" tickMarkClass wid
    "vpXF" : .2
    "vpYF" : .8
    "vpWidthF" : .6
    "vpHeightF" : .6
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
