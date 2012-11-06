;
;    $Id: tx03n.ncl,v 1.13 2010-03-15 22:49:24 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1995                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      File:           tx03n.ncl
;
;      Author:         Bob Lackman
;          National Center for Atmospheric Research
;          PO 3000, Boulder, Colorado
;
;      Date:           Fri Jan 06 18:31:18 MDT 1995
;
;      Description:    Demonstrates the TextItem Object
;                      Writes "NCAR Graphics" in a series of
;                      different colors (using the default colormap.)
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
appid = create "tx03" appClass defaultapp
    "appDefaultParent" : True
    "appUsrDir" : "./"
end create

;
; Default is to create a metafile.
;
wks_type = "ncgm"

if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
  wid = create "tx03Work" ncgmWorkstationClass defaultapp
      "wkMetaName"         : "tx03n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  wid = create "tx03Work" windowWorkstationClass defaultapp
    "wkPause" : True
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "tx03Work" psWorkstationClass defaultapp
    "wkPSFileName"         : "tx03n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "tx03Work" pdfWorkstationClass defaultapp
    "wkPDFFileName"         : "tx03n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wid = create "tx03Work" documentWorkstationClass defaultapp
    "wkFileName"         : "tx03n"
    "wkFormat"         : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wid = create "tx03Work" imageWorkstationClass defaultapp
    "wkFileName"         : "tx03n"
    "wkFormat"         : wks_type
  end create
end if

;
; Get the number of colors in the default color table.
;
getvalues wid
	"wkColorMapLen":  num_colors
end getvalues

;
; Create a TextItem and then draw multiple frames varying the fill
; color of the text bounding box to all entries of the default
; workstation color map.
;


pid = create "TextItems" textItemClass wid
end create
do i=1,num_colors,10
    setvalues pid
        "txBackgroundFillColor" : i
    end setvalues

    draw(pid);
    frame(wid);
end do

;
; End NCL script.
;
end
