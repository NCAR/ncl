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
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;      Date:           Fri Jan 13 18:31:18 MDT 1995
;
;      Description:    Demonstrates the Legend Object defaults.
;

;
; Begin NCL script.
;
begin


;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; In this example the resource file supplies the plot title only.
;
appid = create "lg01" appClass defaultapp
    "appUsrDir" : "./"
end create

;
; Default is to display output to an X workstation
;
wks_type = "x11"

if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
  wid = create "lg01Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "lg01n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  wid = create "lg01Work" windowWorkstationClass defaultapp
    "wkPause" : True
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "lg01Work" psWorkstationClass defaultapp
    "wkPSFileName" : "lg01n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "lg01Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "lg01n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wid = create "lg01Work" documentWorkstationClass defaultapp
    "wkFileName" : "lg01n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wid = create "lg01Work" imageWorkstationClass defaultapp
    "wkFileName" : "lg01n"
    "wkFormat" : wks_type
  end create
end if

;
; Specify the viewport extent of the object.
;
pid = create "Legend" legendClass wid
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

;
; End NCL script.
;
end
