;
;      $Id: nm08n.ncl,v 1.7 2010-03-15 22:49:24 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1998                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  File:         nm08n.ncl
;
;  Author:       Fred Clare
;                National Center for Atmospheric Research
;                PO 3000, Boulder, Colorado
;
;  Date:         Mon Dec  1 12:58:31 MST 1997
;
;  Description:  This program illustrates the use of ftcurvp and ftcurvpi.
;

procedure drw_text(label:string, xpos:float, ypos:float, height:float, \
                   output:graphic)
begin
;
; Draw a text string.
;
label1 = create "Label" textItemClass output
    "txPosXF": xpos
    "txPosYF": ypos
    "txFont": 21
    "txJust" : "centercenter"
    "txString": label
    "txFontHeightF": height
end create
 
draw(label1)
destroy(label1)
 
end

;
; Main program.
;
begin

;
; Specify the input data.
;
xi = (/  0.000, 0.210, 0.360, 0.540, 1.000,    \
         1.500, 1.970, 2.300, 2.500, 2.700  /)
yi = (/  0.000, 2.600, 3.000, 2.500, 0.000,    \
        -1.000, 0.000, 0.800, 0.920, 0.700  /)

;
; Create the output X coordinate array.
;
npts = 201
xr = 5.
xl = -1.
period = 3.
xo = fspan(xl,xr,npts)

;
; Interpolate
;
yo = ftcurvp(xi, yi, period, xo)

;
; Find the integrals starting from x=0.
;
yint = new( (/ npts /), float)
do i = 0, npts-1
  yint(i) = ftcurvpi(0., xo(i), period, xi, yi)
end do

;
; Create a color map.
;
ncolors = 6
cmap = new( (/ncolors,3/), float)
cmap(0,:) = (/1.0,1.0,1.0/)
cmap(1,:) = (/0.0,0.0,0.0/)
cmap(2,:) = (/1.0,0.0,0.0/)
cmap(3,:) = (/0.0,1.0,0.0/)
cmap(4,:) = (/0.0,0.0,1.0/)
cmap(5,:) = (/1.0,1.0,0.0/)

;
; Default is to display output to an X workstation
;
  wks_type = "ncgm"

if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
  xworkid = create "nm08Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "nm08n.ncgm"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  xworkid = create "nm08nWork" windowWorkstationClass defaultapp
    "wkPause" : True
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  xworkid = create "nm08nWork" psWorkstationClass defaultapp
    "wkPSFileName" : "nm08n.ps"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  xworkid = create "nm08nWork" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "nm08n.pdf"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  xworkid = create "nm08nWork" documentWorkstationClass defaultapp
    "wkFileName" : "nm08n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  xworkid = create "nm08nWork" imageWorkstationClass defaultapp
    "wkFileName" : "nm08n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if

;
;
; Draw two graphs: 
;   1.)  The function values (with markers for the input points), 
;   2.)  The integrals (starting from X=0.)
;

;
; Define a data object with the original points.  
;
dataid_original_points = create "xyData" coordArraysClass defaultapp
    "caXArray": xi
    "caYArray": yi
end create

;
; Define a data object with the interpolated points.  
;
dataid_interpolated_points = create "xyData" coordArraysClass defaultapp
    "caXArray": xo
    "caYArray": yo
end create

;
; Define a data object with the integrals.  
;
dataid_integrals = create "xyDatai" coordArraysClass defaultapp
    "caXArray": xo
    "caYArray": yint
end create

;
; Create an immediate mode polyline for marking the Y = 0. line
; in each graph.
;
zero_line_id = create "ZeroLine" graphicStyleClass xworkid
    "gsLineColor": 2
end create
xx = new( (/ 2 /), float)
yy = new( (/ 2 /), float)
xx(0) = xl
xx(1) = xr
yy(0) = 0.
yy(1) = 0.

;
; Create a blank screen plot object.
;
period_legend = create "PeriodLegend" logLinPlotClass xworkid
    "vpXF": 0.13
    "vpYF": 1.00
    "vpWidthF" :  .8
    "vpHeightF" : 1.00
    "trYMaxF" :   6.00
    "trYMinF" :   0.00
    "trXMaxF" :  xr
    "trXMinF" :  xl
end create
;
; Set GraphicStyle resources to modify the immediate mode line attributes.
;
getvalues xworkid
  "wkDefGraphicStyleId": gid
end getvalues

setvalues gid
      "gsLineColor" : 2
end setvalues

ybase = 3.
lbar = xl+1.
off1 = 0.075
off2 = 2.*off1
xa = new( (/ 9 /), float)
ya = new( (/ 9 /), float)
xa(0) = 1.1 
ya(0) = ybase
xa(1) = lbar
ya(1) = ybase
xa(2) = lbar
ya(2) = ybase+off2
xa(3) = xa(1)
ya(3) = ya(1)
xa(4) = lbar+off2
ya(4) = ybase+off1
xa(5) = xa(1)
ya(5) = ya(1)
xa(6) = xa(4)
ya(6) = ybase-off1
xa(7) = xa(1)
ya(7) = ya(1)
xa(8) = lbar
ya(8) = ybase-off2

rbar = lbar+3.
xb = new( (/ 8 /), float)
yb = new( (/ 8 /), float)
xb(0) = 1.90
yb(0) = ybase
xb(1) = rbar
yb(1) = yb(0)
xb(2) = ybase-off2
yb(2) = ybase+off1
xb(3) = xb(1)
yb(3) = yb(1)
xb(4) = xb(2)
yb(4) = yb(1)-off1
xb(5) = xb(1)
yb(5) = yb(1)
xb(6) = rbar
yb(6) = ybase+off2
xb(7) = rbar
yb(7) = yb(0)-off2

;
; Main title.
;
drw_text("Demo for ftcurvp, ftcurvpi",0.5,.95,0.04,xworkid)

;
; Create an XyPlot object for drawing a curve with the interpolated points.
; 
plotid_interpolated_points = create "xyPlot" xyPlotClass xworkid
    "vpXF": .13
    "vpYF": .85
    "vpWidthF" :  .8
    "vpHeightF" : .25
    "trYMaxF" :   3.
    "trYMinF" :  -2.
    "trXMaxF" :  xr
    "trXMinF" :  xl
    "xyCoordData": dataid_interpolated_points
    "tmXTBorderOn": False
    "tmYRBorderOn": False
    "tmXBMinorPerMajor": 4
    "tmYLMinorPerMajor": 0
    "tmBorderThicknessF": 1.
    "tmXMajorGridThicknessF": 1.
    "tmXBMajorLengthF": 0.015
    "tmYLMajorLengthF": 0.015
    "tmXBMinorLengthF": 0.0075
    "tmXBMajorThicknessF": 1.0
    "tmYLMajorThicknessF": 1.0
    "tmXTOn": False
    "tmYROn": False
    "tmXBLabelFont": 21
    "tmXBLabelFontHeightF": 0.025
    "tmYLLabelFont": 21
    "tmYLLabelFontHeightF": 0.025
    "tmYLMode": "Manual"
    "tmYLTickStartF": -2.
    "tmYLTickSpacingF": 1.0
    "tmYLTickEndF": 3.
    "tmXBMode": "Manual"
    "tmXBTickStartF": -1.
    "tmXBTickSpacingF": 1.0
    "tmXBTickEndF": 5.
end create
;
; Add original points.
;
orig_points = NhlAddData(plotid_interpolated_points,"xyCoordData",  \
                         dataid_original_points)
getvalues plotid_interpolated_points
    "xyCoordDataSpec": dataspec
end getvalues
 
setvalues dataspec(1)
    "xyMarkLineMode": "markers"
    "xyMarkerColor": 4
    "xyMarkerSizeF": 0.025
end setvalues

;
; Draw a graph of the interpolated values and mark the original points.
;
draw(plotid_interpolated_points)
drw_text("Function",0.46,.82,0.03,xworkid)
NhlDataPolyline(plotid_interpolated_points, zero_line_id, xx, yy)


;
; Create an XyPlot object for drawing a curve with the integrals.
; 
setvalues plotid_interpolated_points
    "vpYF": 0.37
    "trYMaxF" :  4.0
    "trYMinF" :  -1.0
    "xyCoordData": dataid_integrals
    "tmYLTickStartF": -2.
    "tmYLTickSpacingF": 1.
    "tmYLTickEndF": 4.
end setvalues

;
; Draw the period legend.
;
NhlDataPolyline(period_legend, gid, xb, yb)
NhlDataPolyline(period_legend, gid, xa, ya)
drw_text("Period",0.465,0.5,0.024,xworkid)

;
;
; Draw a graph of the integrals, label the graph,
; and draw a line indicating Y=0.
; 
drw_text("Integral (from X = 0.)",0.4,.35,0.03,xworkid)
NhlDataPolyline(plotid_interpolated_points, zero_line_id, xx, yy)
draw(plotid_interpolated_points)

frame(xworkid)

;
; End NCL script.
;
end
