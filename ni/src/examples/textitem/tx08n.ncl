;
;   $Id: tx08n.ncl,v 1.4 2010-03-15 22:49:24 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      File:           tx08n.ncl
;
;      Author:         Jeff Boote (converted to NCL by Mary Haley)
;                      National Center for Atmospheric Research
;                      PO 3000, Boulder, Colorado
;
;      Date:           Thu Jan 4 10:11:13 MDT 1996
;
;      Description:    Simple annotation example.
;

;
; Begin NCL script.
;
begin

wks_type = "x11"

;
; Create application.
;

if (str_lower(wks_type).eq."ncgm") then
  ixwk = create "tx08Work"  ncgmWorkstationClass defaultapp
    "wkMetaName"         : "tx08n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  ixwk = create "tx08Work" windowWorkstationClass defaultapp end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  ixwk = create "tx08Work"  psWorkstationClass defaultapp
    "wkPSFileName"       : "tx08n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  ixwk = create "tx08Work"  pdfWorkstationClass defaultapp
    "wkPDFFileName"       : "tx08n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  ixwk = create "tx08Work"  documentWorkstationClass defaultapp
    "wkFileName"       : "tx08n"
    "wkFormat"       : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  ixwk = create "tx08Work"  imageWorkstationClass defaultapp
    "wkFileName"       : "tx08n"
    "wkFormat"       : wks_type
  end create
end if

;
; Create Plot object - no data, just illustration annotations.
;
ixyplot = create "xyPlot" xyPlotClass ixwk
  "vpXF":       0.2
  "vpYF":       0.7
  "vpWidthF":   0.3
  "vpHeightF":  0.3
end create

itx = create "txItem" textItemClass ixwk
  "txString":    "Second Line"
end create

ianno = NhlAddAnnotation(ixyplot,itx)
;
; Just set the "zone" to something fairly large, so it is outside
; of all "PlotManager" defined annotations.
; Set the "side" to top - "amJust" defaults to centercenter - but
; setting "amSide" to top makes that effectively bottomcenter.  So,
; the textitem would be drawn so its bottomcenter is placed at the
; top of the viewport of the plot, on the left side.  To get the
; text centered over the plot - set "amParallelPosF" to .5 to move
; the textitem over .5 the width of the plot.
; Set "amOrthogonalPosF" to .1 to give a little bit of spacing
; in the y direction from the plot (the previous zone).
;
setvalues ianno
  "amZone":           10
  "amSide":           "top"
  "amJust":           "bottomcenter"
  "amParallelPosF":    0.5
  "amOrthogonalPosF":  0.1
end setvalues

itx = create "txItem" textItemClass ixwk
  "txString":    "First Line"
end create

ianno = NhlAddAnnotation(ixyplot,itx)
;
; Add this textitem as an annotation with the same charactoristics
; as the first one, but make the zone one higher - so it is just
; outside of the first annotation.  (With a .1 distance away due
; to the "amOrthogonalPosF".
;
setvalues ianno
  "amZone":           11
  "amSide":           "top"
  "amJust":           "bottomcenter"
  "amParallelPosF":   0.5
  "amOrthogonalPosF": 0.1
end setvalues

;
; Draw and advance frame.
;   Notice that drawing the main plot automatically draw's the textitem
;   since it is now a "member plot" of the xyplot.  In fact, you can
;   no-longer draw the textitem indepentently.
;
draw(ixyplot)
frame(ixwk)
;
; Now, if we move the base plot, the annoation stays with the plot.
; It is drawn in its relative position to the xyplot.
;
setvalues ixyplot
  "vpXF":  0.5
  "vpYF":  0.4
end setvalues
draw(ixyplot)
frame(ixwk)

;
; End NCL script.
;
end

