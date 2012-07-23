;
;   $Id: tx02n.ncl,v 1.12 2010-03-15 22:49:24 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      File:           tx02n.ncl
;
;      Author:         Bob Lackman
;                  National Center for Atmospheric Research
;                      PO 3000, Boulder, Colorado
;
;      Date:           Fri Jan 06 18:31:18 MDT 1995
;
;      Description:    Demonstrates the TextItem Object
;                      writing "NCAR Graphics" in a filled
;                      colored font.  Turn on the bounding box.
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
appid = create "tx02" appClass defaultapp
    "appDefaultParent" : True
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
  wid = create "tx02Work" ncgmWorkstationClass defaultapp
      "wkMetaName"         : "tx02n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  wid = create "tx02Work" windowWorkstationClass defaultapp
    "wkPause" : True
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "tx02Work" psWorkstationClass defaultapp
    "wkPSFileName"         : "tx02n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "tx02Work" pdfWorkstationClass defaultapp
    "wkPDFFileName"         : "tx02n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wid = create "tx02Work" documentWorkstationClass defaultapp
    "wkFileName"         : "tx02n"
    "wkFormat"         : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wid = create "tx02Work" imageWorkstationClass defaultapp
    "wkFileName"         : "tx02n"
    "wkFormat"         : wks_type
  end create
end if

;
; Create TextItem object.
;
pid = create "TextItems" textItemClass wid end create

draw(pid)
frame(wid)

;
; End NCL script.
;
end
