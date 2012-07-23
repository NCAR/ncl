;
;   $Id: ti03n.ncl,v 1.12 2010-03-15 22:49:25 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      File:           ti03n.ncl
;
;      Author:         Bob Lackman (converted by Ed Stautler)
;          National Center for Atmospheric Research
;          PO 3000, Boulder, Colorado
;
;      Date:           Fri Jan 06 18:31:18 MDT 1995
;
;      Description:    Demonstrates Title Object resource changes.
;

;
; Begin NCL script.
;
begin

;
; Modify the color map.  Color indices '0' and '1' are the background
; and foreground colors respectively.
;
cmap = (/ (/0.0,0.0,0.0/), \
	 (/0.0,1.0,1.0/), \
	 (/1.0,0.5,0.0/), \
	 (/1.0,1.0,0.0/)/)

;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; In this example the resource file supplies the plot title only.
;
appid = create "ti03" appClass defaultapp
    "appDefaultParent" : True
    "appUsrDir" : "."
end create

;
; Default is to display output to an X workstation
;
wks_type = "x11"

if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
  wid = create "ti03Work" ncgmWorkstationClass defaultapp
      "wkColorMap" : cmap
      "wkMetaName"         : "ti03n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  wid = create "ti03Work" windowWorkstationClass defaultapp
    "wkColorMap" : cmap
    "wkPause" : True
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "ti03Work" psWorkstationClass defaultapp
    "wkColorMap"    : cmap
    "wkPSFileName"  : "ti03n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "ti03Work" pdfWorkstationClass defaultapp
    "wkColorMap"    : cmap
    "wkPDFFileName"  : "ti03n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wid = create "ti03Work" documentWorkstationClass defaultapp
    "wkColorMap"    : cmap
    "wkFileName"  : "ti03n"
    "wkFormat"  : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wid = create "ti03Work" imageWorkstationClass defaultapp
    "wkColorMap"    : cmap
    "wkFileName"  : "ti03n"
    "wkFormat"  : wks_type
  end create
end if

;
; Specify the viewport extent of the object.
;
pid = create "Titles" titleClass wid
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

;
; End NCL script.
;
end
