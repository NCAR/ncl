;
;      $Id: nm07n.ncl,v 1.7 2010-03-15 22:49:24 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1998                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  File:         nm07n.ncl
;
;  Author:       Fred Clare
;                National Center for Atmospheric Research
;                PO 3000, Boulder, Colorado
;
;  Date:         Mon Dec  1 12:58:31 MST 1997
;
;  Description:  This program illustrates the use of ftcurv, ftcurvd,
;                and ftcurvi.
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
update(output)
destroy(label1)
 
end

;
; Main program.
;
begin

;
; Specify the input data.
;
xi = (/  0.00,   2.00,   5.00,   8.00,  10.00,  13.00,     \
        15.00,  18.00,  21.00,  23.00,  30.00         /)
yi = (/  1.00,   0.81,   0.00,  -0.81,  -1.00,  -0.84,     \
        -0.56,   0.04,   0.73,   1.18,   2.0          /)

;
; Create the output X coordinate array.
;
npts = 201
xr = 30.
xo = fspan(0.,xr,npts)

;
; Require the derivatives at the endpoints to be zero.
;
ftsetp("sf1",0)
ftsetp("sl1", 0.0)
ftsetp("sln", 0.0)

;
; Interpolate
;
yo = ftcurv(xi, yi, xo)

;
; Find the derivatives.
;
yd = ftcurvd(xi, yi, xo)

;
; Find the integrals starting from x=0.
;
yint = new( (/ npts /), float)
xinc = xr/(npts-1)
do i = 0, npts-1
  yint(i) = ftcurvi(0., xinc*i, xi, yi)
end do

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
  xworkid = create "nm07Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "nm07n.ncgm"
      "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  xworkid = create "nm07Work" windowWorkstationClass defaultapp
    "wkPause" : True
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  xworkid = create "nm07Work" psWorkstationClass defaultapp
    "wkPSFileName" : "nm07n.ps"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  xworkid = create "nm07Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "nm07n.pdf"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  xworkid = create "nm07Work" documentWorkstationClass defaultapp
    "wkFileName" : "nm07n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  xworkid = create "nm07Work" imageWorkstationClass defaultapp
    "wkFileName" : "nm07n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if

;
;
; Draw three graphs: 
;   1.)  The function values (with markers for the input points), 
;   2.)  The derivatives,
;   3.)  The integrals (starting from X=0.)
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
; Define a data object with the interpolated points.  The id for this 
; object will later be used as the value for an XYPlot data resource, 
; 'xyCoordData'.
;
dataid_interpolated_points = create "xyData" coordArraysClass defaultapp
    "caXArray": xo
    "caYArray": yo
end create

;
; Define a data object with the derivatives.  The id for this 
; object will later be used as the value for an XYPlot data resource, 
; 'xyCoordData'.
;
dataid_derivatives = create "xyData" coordArraysClass defaultapp
    "caXArray": xo
    "caYArray": yd
end create

;
; Define a data object with the integrals.  The id for this 
; object will later be used as the value for an XYPlot data resource, 
; 'xyCoordData'.
;
dataid_integrals = create "xyDatai" coordArraysClass defaultapp
    "caXArray": xo
    "caYArray": yint
end create

;
; Specify a point of origin for three graphs.
;
ypos_top = 0.88

;
; Main title.
;
drw_text("Demo for ftcurv, ftcurvd, ftcurvi",0.5,.95,0.04,xworkid)

;
; Create an immediate mode polyline for marking the Y = 0. line
; in each graph.
;
zero_line_id = create "ZeroLine" graphicStyleClass xworkid
    "gsLineColor": 2
end create
xx = (/0.,xr/)
yy = (/0., 0./)

;
; Create an XyPlot object for drawing a curve with the interpolated
; points.  This is created as a child of the XWorkstation object.  
; 
plotid_interpolated_points = create "xyPlot" xyPlotClass xworkid
    "vpXF": .13
    "vpYF": ypos_top
    "vpWidthF" :  .8
    "vpHeightF" : .2
    "trYMaxF" :  2.0
    "trYMinF" : -1.0
    "trXMaxF" : xr
    "trXMinF" :  0.0
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
    "tmYLTickStartF": -1.0
    "tmYLTickSpacingF": 1.0
    "tmYLTickEndF": 2.0
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
drw_text("Function",0.4,ypos_top - 0.03,0.035,xworkid)
NhlDataPolyline(plotid_interpolated_points, zero_line_id, xx, yy)

;
; Create an XyPlot object for drawing a curve with the derivative points.
; 
setvalues plotid_interpolated_points
    "vpYF": ypos_top - 0.3
    "trYMaxF" :  0.35
    "trYMinF" : -0.35
    "xyCoordData": dataid_derivatives
    "tmYLTickStartF": -0.3
    "tmYLTickSpacingF": 0.2
    "tmYLTickEndF": 0.3
end setvalues

;
; Draw the graph of the derivatives.
;
;
draw(plotid_interpolated_points)
drw_text("Derivative",0.3,ypos_top - 0.33,0.035,xworkid)
NhlDataPolyline(plotid_interpolated_points, zero_line_id, xx, yy)

;
; Create an XyPlot object for drawing a curve with the integrals.
; 
setvalues plotid_interpolated_points
    "vpYF": ypos_top - 0.6
    "trYMaxF" :  10.
    "trYMinF" :  -6.
    "xyCoordData": dataid_integrals
    "tmYLTickStartF": -6.
    "tmYLTickSpacingF": 4.
    "tmYLTickEndF": 10.
end setvalues

;
; Draw the graph of the integrals.
;
;
draw(plotid_interpolated_points)
drw_text("Integral",0.3,ypos_top - 0.63,0.035,xworkid)
NhlDataPolyline(plotid_interpolated_points, zero_line_id, xx, yy)

frame(xworkid)

;
; End NCL script.
;
end
