;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      File:           tm04n.ncl
;
;      Author:         David Brown
;          	       National Center for Atmospheric Research
;          	       PO 3000, Boulder, Colorado
;
;      Date:           Thu Jun 29 14:42:04 MDT 1995
;
;      Description:    Demonstrates format options for TickMark labels
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
appid = create "tm04" appClass defaultapp
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
  wid = create "tm04Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "tm04n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  wid = create "tm04Work" windowWorkstationClass defaultapp
    "wkPause" : True
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an PS object.
;
  wid = create "tm04Work" psWorkstationClass defaultapp
    "wkPSFileName" : "tm04n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an PDF object.
;
  wid = create "tm04Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "tm04n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wid = create "tm04Work" documentWorkstationClass defaultapp
    "wkFileName" : "tm04n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wid = create "tm04Work" imageWorkstationClass defaultapp
    "wkFileName" : "tm04n"
    "wkFormat" : wks_type
  end create
end if

;
; Create a title for the complete frame using a text item
;
tid = create "TitleText" textItemClass wid
    "txFont" : "helvetica-bold"
    "txString" : "Format String"
    "txJust" : "bottomright"
    "txPosYF" : .95
    "txPosXF" : .19
    "txFontHeightF" : 0.02
end create
draw(tid)

setvalues tid 
    "txString" : "Resulting TickMark Labels"
    "txJust" : "bottomcenter"
    "txPosYF" : .95
    "txPosXF" : .55
end setvalues
draw(tid)

;
; Create a LogLinPlot to serve only as a frame around which the
; explanatory titles and the example ticks and TickMark labels 
; are positioned.
; Turn off all TickMark axes and borders except the bottom X-Axis.
; The main title is used to characterize the output.
; The y-axis title contains the format string used to generate the labels.
; Specify the viewport extent of the object.
;

pid = create "FormatDemo" logLinPlotClass wid
    "vpXF" : .2
    "vpYF" : .9
    "vpWidthF" : .7
    "vpHeightF" : .02
    "pmTickMarkDisplayMode" : "always"
    "pmTitleDisplayMode" : "always"
    "pmTitleZone" : 2
    "tmXBMajorLengthF" : 0.02
    "tmXBMinorLengthF" : 0.01
    "tmXBMinorPerMajor" : 4
    "tmXTBorderOn": False
    "tmYLBorderOn": False
    "tmYRBorderOn": False
    "tmXTOn": False
    "tmYLOn": False
    "tmYROn": False
    "tiMainPosition" : "left"
    "tiMainJust" : "centerleft"
    "tiMainFuncCode" : "~"
    "tiYAxisFuncCode" : "\"
    "tiYAxisAngleF" : 0.0
    "tiYAxisJust" : "BottomRight"
end create

;
; Draw 9 different plots demonstrating control of the bottom x-axis 
; TickMark labels using the XBFormat string resource. See the description
; of the Floating Point Format Specification scheme in the HLU reference
; guide to learn about the semantics and syntax of the format string.
; There are links to this description in the TickMark reference pages under
; the entries for the format string resources (XBFormat, for example).
;

setvalues pid
    "vpYF" : .9
    "tiMainString" : "Default format"
    "tiYAxisString" : "0@*+^sg"
end setvalues
draw(pid)

setvalues pid
    "vpYF" : .8
    "tiMainString" : "Equal number of significant digits"
    "tiYAxisString" : "0f"
    "tmXBFormat" : "0f" 
end setvalues
draw(pid)

setvalues pid
    "vpYF" : .7
    "tiMainString" : "No unnecessary zeroes"
    "tiYAxisString" : "f"
    "tmXBFormat" : "f" 
end setvalues
draw(pid)

setvalues pid
    "vpYF" : .6
    "tiMainString" : "Force decimal point"
    "tiYAxisString" : "#f"
    "tmXBFormat" : "#f" 
end setvalues
draw(pid)

;
; Note that when the XBFormat string specifies the precision (number
; of significant digits) explicitly (using the '.' conversion field),
; both XBAutoPrecision and XBPrecision are ignored.
;

setvalues pid
    "vpYF" : .5
    "tiMainString" : "4 significant digits for maximum absolute value"
    "tiYAxisString" : "0@;*.4f"
    "tmXBFormat" : "0@;*.4f" 
end setvalues
draw(pid)

setvalues pid
    "vpYF" : .4
    "tiMainString" : \
	"Zero fill 5 character field"
    "tiYAxisString" : "0@5;*.4f"
    "tmXBFormat" : "0@5;*.4f" 
end setvalues
draw(pid)

setvalues pid
    "vpYF" : .3
    "tiMainString" : \
	"Field width: 7; fill character: '*'; decimal position: 4"
    "tiYAxisString" : "&*0@7;*.4~4f" 
    "tmXBFormat" : "&*0@7;*.4~4f" 
end setvalues
draw(pid)

;
; Note that the tick spacing is set to a larger value because 
; the exponential notation takes up more space.
;

setvalues pid
    "vpYF" : .2
    "tiMainString" : \
      "Exponential format using superscript notation"
    "tmXBTickSpacingF" : 5.0
    "tiYAxisString" : "0@!;*^se" 
    "tmXBFormat" : "0@!;*^se" 
end setvalues
draw(pid)

setvalues pid
    "vpYF" : .1
    "tiMainString" : "Exponential format using '**' notation"
    "tiYAxisString" : "0@!;*^ae" 
    "tmXBFormat" : "0@!;*^ae" 
end setvalues
draw(pid)

frame(wid)
delete(pid)
delete(wid)
delete(appid)

;
; End NCL script.
;
end
