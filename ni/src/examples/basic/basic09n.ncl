;
; $Id: basic09n.ncl,v 1.1 1996-07-09 23:54:33 haley Exp $
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

;
; 'min' function
;
function min(i:integer,j:integer)
begin  
  if (i .le. j) then
    return(i)
  else
    return(j)
  end if
end

;
; Set the display. Default is to display output to an X workstation
;
NCGM=0
X11=1
PS=0

if (NCGM .eq. 1) then
;
; Create an ncgmWorkstation object.
;
  wks = create "wks" ncgmWorkstationClass defaultapp
      "wkMetaName" : "basic09n.ncgm"
  end create
else
  if (X11 .eq. 1) then
;
; Create an XWorkstation object.
;
    wks = create "wks" xWorkstationClass defaultapp
      "wkPause" : "True"
    end create
  else
    if (PS .eq. 1) then
;
; Create a PSWorkstation object.
;
      wks = create "wks" psWorkstationClass defaultapp
        "wkPSFileName" : "basic09n.ps"
      end create
    end if
  end if
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
mod = num_fonts % num_lines
if (mod .ne. 0) then
  div = div +1
end if

txid = create "TextItem" textItemClass wks
  "txFuncCode" : "*"
  "txPosXF" : 0.5
  "txPosYF" : 0.98
  "txFontHeightF" : 0.02
  "txString" : "font number : font name     what it looks like"
end create

do j = 1,div
  k = 0.90
  do i=(j-1)*num_lines,min(num_fonts-1,j*num_lines-1)
    txid1 = create "TextItem1" textItemClass wks
      "txJust" : 2
      "txFuncCode" : "*"
      "txPosXF" : 0.1
      "txPosYF" : k
      "txFontHeightF" : 0.02
      "txString" : fonts(i,0) + " : " + fonts(i,1)
    end create
    txid2 = create "TextItem2" textItemClass wks
      "txJust" : 2
      "txFont" : fonts(i,1)
      "txString" : "NCAR Graphics"
      "txPosXF" : 0.6
      "txPosYF" : k
      "txFontHeightF" : 0.02
    end create
    draw(txid)
    draw(txid1)
    draw(txid2)
    delete(txid1)
    delete(txid2)
    k = k - .05
  end do
  frame(wks)
end do

;
; Clean up;
;
delete(wks)

;
; End the ncl script.
;
end
