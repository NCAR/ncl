;
;   $Id: tx06n.ncl,v 1.10 2010-03-15 22:49:24 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      File:           tx06n.ncl
;
;      Author:         Fred Clare
;                      National Center for Atmospheric Research
;                      PO 3000, Boulder, Colorado
;
;      Date:           Thu Mar 16 16:01:13 MST 1995
;
;      Description:    Demonstrates TextItem text justifications.
;

;
; Begin NCL script.
;
begin

;
; Create a workstation, defining the desired colors.
;
cmap = (/ (/1.0, 1.0, 1.0/), \
	 (/0.0, 0.0, 1.0/), \
	 (/1.0, 0.0, 0.4/), \
	 (/0.4, 0.0, 0.4/) /)
;
; Default is to display output to an X workstation
;
wks_type = "x11"

if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
  workstation_id = create "tx06Work" ncgmWorkstationClass defaultapp
      "wkMetaName"         : "tx06n.ncgm"
      "wkColorMap"         : cmap
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  workstation_id = create "tx06Work"  windowWorkstationClass defaultapp
    "wkPause"            : True
    "wkColorMap"         : cmap
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  workstation_id = create "tx06Work" psWorkstationClass defaultapp
    "wkPSFileName"       : "tx06n.ps"
    "wkColorMap"         : cmap
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  workstation_id = create "tx06Work" pdfWorkstationClass defaultapp
    "wkPDFFileName"       : "tx06n.pdf"
    "wkColorMap"         : cmap
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  workstation_id = create "tx06Work" documentWorkstationClass defaultapp
    "wkFileName"       : "tx06n"
    "wkFormat"       : wks_type
    "wkColorMap"         : cmap
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  workstation_id = create "tx06Work" imageWorkstationClass defaultapp
    "wkFileName"       : "tx06n"
    "wkFormat"       : wks_type
    "wkColorMap"         : cmap
  end create
end if

;
; Create a TextItem object.
;
text_item_id = create "tx06TextItem" textItemClass workstation_id
end create

labels = (/                                                  \
            "Top Left"  ,"Center Left" , "Bottom Left"  ,    \
            "Top Center" ,"Center Center", "Bottom Center",  \
            "Top Right"  ,"Center Right" , "Bottom Right"    \
          /)

;
;  Draw a string with various justifications.
;
char_height = 0.035
Xmarker_height = .03
do just = 0, 8
  y_coord =  0.08*just+0.1
  if ( (just % 3) .eq. 0) then
    y_coord = y_coord + 0.45*char_height
  end if
  if ( (just % 3) .eq. 2) then
    y_coord = y_coord - 0.45*char_height
  end if
  setvalues text_item_id
    "txString" : labels(just)
    "txFontColor" : 1
    "txJust" : just
    "txFont" : 22
    "txPosXF" : 0.5
    "txFontHeightF" : char_height
    "txPosYF" : y_coord
  end setvalues
  draw(text_item_id)

;
;  Mark the justification point.
;
  setvalues text_item_id
    "txString" : "X"
    "txFontColor" : 2
    "txJust" : 4
    "txFont" : 22
    "txFontHeightF" : Xmarker_height
  end setvalues
  draw(text_item_id)
end do

;
; Label the plot.
;
setvalues text_item_id
  "txFont" : 25
  "txJust" : 4
  "txFontColor" : 3
  "txFontHeightF" : 0.045
  "txPosXF" : 0.5
  "txPosYF" : 0.92
  "txString" : "Text Justifications"
end setvalues
draw(text_item_id)

setvalues text_item_id
  "txPosXF" : 0.5+0.5*char_height
  "txPosYF" : 0.84
  "txString" : " - Marks the justification point"
end setvalues
draw(text_item_id)

bb = NhlGetBB(text_item_id)
setvalues text_item_id
  "txFont" : 22
  "txFontColor" : 2
  "txFontHeightF" : Xmarker_height
  "txPosXF" : bb(2)
  "txPosYF" : 0.837
  "txJust" : 7
  "txString" : "X"
end setvalues
draw(text_item_id)

frame(workstation_id)

delete(text_item_id)
delete(workstation_id)

;
; End NCL script.
;
end
