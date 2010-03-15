;
;   $Id: ti03n.ncl,v 1.11 2010-03-15 15:05:01 haley Exp $
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
else
  if (str_lower(wks_type).eq."x11") then
;
; Create an XWorkstation object.
;
    wid = create "ti03Work" xWorkstationClass defaultapp
      "wkColorMap" : cmap
      "wkPause" : True
    end create
  else
    if (str_lower(wks_type).eq."ps") then
;
; Create a PSWorkstation object.
;
      wid = create "ti03Work" psWorkstationClass defaultapp
        "wkColorMap"    : cmap
        "wkPSFileName"  : "ti03n.ps"
      end create
    else
      if (str_lower(wks_type).eq."pdf") then
;
; Create a PDFWorkstation object.
;
        wid = create "ti03Work" pdfWorkstationClass defaultapp
          "wkColorMap"    : cmap
          "wkPDFFileName"  : "ti03n.pdf"
        end create
      end if
    end if
  end if
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
