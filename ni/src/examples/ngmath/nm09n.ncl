;
;      $Id: nm09n.ncl,v 1.7 2010-03-15 22:49:24 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1998                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  File:         nm09n.ncl
;
;  Author:       Fred Clare
;                National Center for Atmospheric Research
;                PO 3000, Boulder, Colorado
;
;  Date:         Wed Jan  7 10:52:47 MST 1998
;
;  Description:  This program illustrates the use of ftcurvs and ftcurvps.
;

;
; Main program
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
d = 0.3
yos = ftcurvs(xi, yi, d, xo)

;
; Same as above, except for a periodic function.
;
yosp = ftcurvps(xi, yi, period, d, xo)

;
; Create a color map.
;
cmap = (/ (/1.0,1.0,1.0/), \
	 (/0.0,0.0,0.0/), \
	 (/1.0,0.0,0.0/), \
	 (/0.0,1.0,0.0/), \
	 (/0.0,0.0,1.0/), \
	 (/1.0,1.0,0.0/) /)

;
; Default is to display output to an X workstation
;
  wks_type = "ncgm"

if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
  xworkid = create "nm09Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "nm09n.ncgm"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  xworkid = create "nm09Work" windowWorkstationClass defaultapp
    "wkPause" : "True"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  xworkid = create "nm09Work" psWorkstationClass defaultapp
    "wkPSFileName" : "nm09n.ps"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  xworkid = create "nm09Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "nm09n.pdf"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  xworkid = create "nm09Work" documentWorkstationClass defaultapp
    "wkFileName" : "nm09n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  xworkid = create "nm09Work" imageWorkstationClass defaultapp
    "wkFileName" : "nm09n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if

;
;
; Draw two graphs: 
;   1.)  The function values for the non-periodic curve.
;   2.)  The function values for the periodic curve.
;

;
; Define a data object with the original points.  The id for this 
; object will later be used as the value for an XYPlot data resource, 
; 'xyCoordData'.
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
    "caYArray": yos
end create

;
; Define a data object with the functional values for the periodic function.
;
dataid_periodic = create "xyDatai" coordArraysClass defaultapp
    "caXArray": xo
    "caYArray": yosp
end create

;
; Specify a point of origin for the graphs.
;
ypos_top = 0.95

;
; Create an immediate mode polyline for marking the Y = 0. line
; in each graph.
;
zero_line_id = create "ZeroLine" graphicStyleClass xworkid
    "gsLineColor": 2
end create
xx = (/xl,xr/)
yy = (/0.0,0.0/)

;
; Create a label for the graph of function values of the non-periodic
; function.
;
label1 = create "FunctionLabel" textItemClass xworkid
    "txPosXF": 0.42
    "txPosYF": ypos_top - 0.05
    "txFont": 21
    "txJust" : "centerleft"
    "txString": "ftcurvs"
    "txFontHeightF": 0.035
end create
;
; Create an XyPlot object for drawing a curve with the interpolated
; points for the non-periodic function. 
; 
plotid_interpolated_points = create "xyPlot" xyPlotClass xworkid
    "vpXF": .13
    "vpYF": ypos_top
    "vpWidthF" :  .8
    "vpHeightF" : .35
    "trYMaxF" :   4.
    "trYMinF" :  -2.
    "trXMaxF" :  xr
    "trXMinF" :  xl
    "xyCoordData": dataid_interpolated_points
    "tmXTBorderOn": "False"
    "tmYRBorderOn": "False"
    "tmXBMinorPerMajor": 4
    "tmYLMinorPerMajor": 0
    "tmBorderThicknessF": 1.
    "tmXMajorGridThicknessF": 1.
    "tmXBMajorLengthF": 0.015
    "tmYLMajorLengthF": 0.015
    "tmXBMinorLengthF": 0.0075
    "tmXBMajorThicknessF": 1.0
    "tmYLMajorThicknessF": 1.0
    "tmXTOn": "False"
    "tmYROn": "False"
    "tmXBLabelFont": 21
    "tmXBLabelFontHeightF": 0.025
    "tmYLLabelFont": 21
    "tmYLLabelFontHeightF": 0.025
    "tmYLMode": "Manual"
    "tmYLTickStartF": -2.
    "tmYLTickSpacingF": 2.0
    "tmYLTickEndF": 4.
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
; Create a label for the graph of the periodic function.
;
label3 = create "IntegralLabel" textItemClass xworkid
    "txPosXF": 0.42
    "txPosYF": ypos_top - 0.55
    "txFont": 21
    "txJust" : "centerleft"
    "txString": "ftcurvps"
    "txFontHeightF": 0.035
end create

;
;  Draw the curve for the non-periodic function and mark the original points.
;
draw(plotid_interpolated_points)
draw(label1)
NhlDataPolyline(plotid_interpolated_points, zero_line_id, xx, yy)

;
; Create an XyPlot object for drawing a curve with the interpolated
; values of the periodic function. 
; 
setvalues plotid_interpolated_points
    "vpYF": ypos_top - 0.5
    "trYMaxF" :  4.0
    "trYMinF" :  -2.0
    "xyCoordData": dataid_periodic
    "tmYLTickStartF": -2.
    "tmYLTickSpacingF": 2.0
    "tmYLTickEndF": 4.
end setvalues

;
; Draw a graph of the periodic function, label the graph,
; and draw a line indicating Y=0.
; 
draw(plotid_interpolated_points)
draw(label3)
NhlDataPolyline(plotid_interpolated_points, zero_line_id, xx, yy)

frame(xworkid)

;
; End NCL script.
;
end
