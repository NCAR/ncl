;
; $Id: basic09n.ncl,v 1.8 2010-03-15 22:49:23 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1996                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       basic09n.ncl
;
;   Author: Mary Haley
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;
;   Date: Tue Jul  9 16:44:42 MDT 1996
;
;   Description: This example displays all the available fonts
;

begin

wks_type = "x11"

if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
  wid = create "wks" ncgmWorkstationClass defaultapp
      "wkMetaName" : "basic09n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  wid = create "wks" windowWorkstationClass defaultapp
    "wkPause" : "True"
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "wks" psWorkstationClass defaultapp
    "wkPSFileName" : "basic09n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "wks" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "basic09n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wid = create "wks" documentWorkstationClass defaultapp
    "wkFileName" : "basic09n"
    "wkFormat"   : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wid = create "wks" imageWorkstationClass defaultapp
    "wkFileName" : "basic09n"
    "wkFormat"   : wks_type
  end create
end if

;
;  List of available fonts (by number and name)
;

fonts = (/(/"1","default"/),\
        (/"2","cartographic_roman"/),\
        (/"3","cartographic_greek"/),\
        (/"4","simplex_roman"/),\
        (/"5","simplex_greek"/),\
        (/"6","simplex_script"/),\
        (/"7","complex_roman"/),\
        (/"8","complex_greek"/),\
        (/"9","complex_script"/),\
        (/"10","complex_italic"/),\
        (/"11","complex_cyrillic"/),\
        (/"12","duplex_roman"/),\
        (/"13","triplex_roman"/),\
        (/"14","triplex_italic"/),\
        (/"15","gothic_german"/),\
        (/"16","gothic_english"/),\
        (/"17","gothic_italian"/),\
        (/"18","math_symbols"/),\
        (/"19","symbol_set1"/),\
        (/"20","symbol_set2"/),\
        (/"21","helvetica"/),\
        (/"22","helvetica-bold"/),\
        (/"25","times-roman"/),\
        (/"26","times-bold"/),\
        (/"29","courier"/),\
        (/"30","courier-bold"/),\
        (/"33","greek"/),\
        (/"34","math-symbols"/),\
        (/"35","text-symbols"/),\
        (/"36","weather1"/),\
        (/"37","weather2"/)/)

sizes = dimsizes(fonts)

; 
; We only want 18 lines on a page
;
num_lines = 18
num_fonts = sizes(0)

div = new(1,integer)
div = num_fonts / num_lines
mod1 = num_fonts % num_lines
if (mod1 .ne. 0) then
  div = div +1
end if

;
; Create header to put on all frames
;
txid1 = create "TextItem1" textItemClass wid
  "txFuncCode" : "*"
  "txPosXF" : 0.32
  "txPosYF" : 0.98
  "txFontHeightF" : 0.02
  "txFontColor" : 3
  "txString" : "font number : font name"
end create

txid2 = create "TextItem2" textItemClass wid
  "txPosXF" : 0.72
  "txPosYF" : 0.98
  "txFontHeightF" : 0.02
  "txFontColor" : 5
  "txString" : "font example"
end create

do j = 1,div
  k = 0.90
  draw(txid1)
  draw(txid2)
;
; Loop over each font
;
  do i=(j-1)*num_lines,min((/num_fonts-1,j*num_lines-1/))
;
; Font number and name
;
    txid3 = create "TextItem3" textItemClass wid
      "txJust" : 2
      "txFuncCode" : "*"
      "txPosXF" : 0.1
      "txPosYF" : k
      "txFontHeightF" : 0.02
      "txFontColor" : 3
      "txString" : fonts(i,0) + " : " + fonts(i,1)
    end create
;
; Actual font drawn using the words 'NCAR Graphics'
;
    txid4 = create "TextItem4" textItemClass wid
      "txJust" : 2
      "txFont" : fonts(i,1)
      "txString" : "NCAR Graphics"
      "txPosXF" : 0.6
      "txPosYF" : k
      "txFontColor" : 5
      "txFontHeightF" : 0.02
    end create
    draw(txid3)
    draw(txid4)
    delete(txid3)
    delete(txid4)
    k = k - .05
  end do
  frame(wid)
end do

;
; Clean up;
;
delete(wid)

;
; End the ncl script.
;
end
