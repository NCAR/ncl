;
;      $Id: nm11n.ncl,v 1.5 2010-03-15 22:49:24 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1998                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  File:         nm11n.ncl
;
;  Author:       Fred Clare
;                National Center for Atmospheric Research
;                PO 3000, Boulder, Colorado
;
;  Date:         Fri Jan  9 17:16:41 MST 1998
;
;  Description:  This program illustrates the use of ftkurvp and ftkurvpd.
;
procedure drw_text(label:string, xpos:float, ypos:float, height:float, \
                   output:graphic)
begin
;
; Create a label for the graph of function values.
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
; Main program
;
begin
;
; Specify the input data.
;
x = (/ 13.0,  9.0,  9.0, 16.0, 21.0, 27.0,    \
       34.0, 36.0, 34.0, 26.0, 19.5         /)
y = (/ 35.0, 31.0, 18.0, 12.0,  9.6,  8.4,    \
       13.2, 21.6, 30.0, 37.2, 37.4         /)

;
; Create an array of parameterized values where interpolated values
; are desired.
;
npts = 201
u = fspan(0.,1.,npts)

;
; Interpolate.
;
xo = new( (/ npts /), float)
yo = new( (/ npts /), float)
ftkurvp(x, y, u, xo, yo)

;
;  Get the derivatives (this returns the interpolsted values as well).
;
xs = new( (/ npts /), float)
ys = new( (/ npts /), float)
xd = new( (/ npts /), float)
yd = new( (/ npts /), float)
xdd = new( (/ npts /), float)
ydd = new( (/ npts /), float)
ftkurvpd(x, y, u, xs, ys, xd, yd, xdd, ydd)

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
  xworkid = create "nm11Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "nm11n.ncgm"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  xworkid = create "nm11Work" windowWorkstationClass defaultapp
    "wkPause" : True
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  xworkid = create "nm11Work" psWorkstationClass defaultapp
    "wkPSFileName" : "nm11n.ps"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  xworkid = create "nm11Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "nm11n.pdf"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  xworkid = create "nm11Work" documentWorkstationClass defaultapp
    "wkFileName" : "nm11n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  xworkid = create "nm11Work" imageWorkstationClass defaultapp
    "wkFileName" : "nm11n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if

;
;
; Draw two graphs: 
;   1.)  The interpolated function values.
;   2.)  The first derivatives with respect to the parameter variable.
;

;
; Define data objects:
;   dataid1(0) - coordinates of the original points.
;   dataid1(1) - coordinates of the interpolated points.
;   dataid2(0) - zero line for derivative plots.
;   dataid2(1) - coordinates of the first derivative wrt u.
;   dataid3(0) - zero line for derivative plots.
;   dataid3(1) - coordinates of the first derivative wrt u.
;
dataid1 = new(2,graphic)
dataid2 = new(2,graphic)
dataid3 = new(2,graphic)

; Define a data object with the original points.  The id for this 
; object will later be used as the value for an XYPlot data resource, 
; 'xyCoordData'.
;
dataid1(0) = create "OriginalPoints" coordArraysClass defaultapp
    "caXArray": x
    "caYArray": y
end create

;
; Define a data object with the interpolated points.  The id for this 
; object will later be used as the value for an XYPlot data resource, 
; 'xyCoordData'.
;
dataid1(1) = create "InterpolatedPoints" coordArraysClass defaultapp
    "caXArray": xo
    "caYArray": yo
end create

;
; Define a data object to mark the zero line in the derivative plots.
;
xz = new(2,float)
yz = new(2,float)
xz(0) = 0.
xz(1) = 1.
yz(0) = 0.
yz(1) = 0.
dataid2(0) = create "ZeroLine" coordArraysClass defaultapp
    "caXArray": xz
    "caYArray": yz
end create
dataid3(0) = dataid2(0)

;
; Define a data object with the derivatives of X wrt the parameter
; variable. 
;
dataid2(1) = create "XDerivatives" coordArraysClass defaultapp
    "caXArray": u
    "caYArray": xd
end create

;
; Define a data object with the derivatives of X wrt the parameter
; variable. 
;
dataid3(1) = create "YDerivatives" coordArraysClass defaultapp
    "caXArray": u
    "caYArray": yd
end create

;
; Create an XyPlot object for drawing a curve with the interpolated
; points.
; 
plotid_function = create "xyPlot" xyPlotClass xworkid
    "vpXF": .15
    "vpYF": .85
    "vpWidthF" :  .7
    "vpHeightF" : .7
    "trYMaxF" :   40.
    "trYMinF" :   0.
    "trXMaxF" :   40.
    "trXMinF" :   0.
    "xyCoordData": dataid1
    "tmXTBorderOn": False
    "tmYRBorderOn": False
    "tmXBMinorPerMajor": 5
    "tmYLMinorPerMajor": 5
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
    "tmYLTickStartF": 0.
    "tmYLTickSpacingF": 10.0
    "tmYLTickEndF": 40.
    "tmXBMode": "Manual"
    "tmXBTickStartF": 0.
    "tmXBTickSpacingF": 10.0
    "tmXBTickEndF": 40.
end create

getvalues plotid_function
    "xyCoordDataSpec": dataspec
end getvalues

setvalues dataspec(0)
    "xyMarkLineMode": "markers"
    "xyMarkerColor": 4
    "xyMarkerSizeF": 0.025
end setvalues

;
; Draw a graph of the interpolated points.
; 
draw(plotid_function)
drw_text("Demo for ftkurvp",0.5,0.93,0.04,xworkid)
frame(xworkid)

;
; Draw a graph of the X derivative.
;
setvalues plotid_function
    "trYMaxF" :   100.
    "trYMinF" :  -100.
    "trXMaxF" :   1.
    "trXMinF" :   0.
    "xyCoordData": dataid2
    "tmYLTickStartF": -100.
    "tmYLTickSpacingF": 50.0
    "tmYLTickEndF": 100.
    "tmXBTickStartF": 0.
    "tmXBTickSpacingF": 0.2
    "tmXBTickEndF": 1.
    "tmXBMajorLengthF": 0.020
    "tmXBMinorLengthF": 0.010
    "tmYLMajorLengthF": 0.020
    "tmYLMinorLengthF": 0.010
    "tmXBMinorPerMajor": 4
    "tmYLMinorPerMajor": 4
    "vpXF": .15
    "vpYF": .87
    "vpWidthF" :  .7
    "vpHeightF" : .32
    "tmYLLabelFontHeightF": 0.023
    "tmXBLabelFontHeightF": 0.023
end setvalues
getvalues plotid_function
    "xyCoordDataSpec": dataspec
end getvalues
setvalues dataspec(0)
    "xyLineColor": 2
end setvalues
draw(plotid_function)

;
; Draw a graph of the Y derivative.
;
setvalues plotid_function
    "trYMinF" :  -100.
    "xyCoordData": dataid3
    "tmYLTickStartF": -100.
    "vpXF": .15
    "vpYF": .42
    "vpHeightF" : .32
end setvalues
getvalues plotid_function
    "xyCoordDataSpec": dataspec
end getvalues
setvalues dataspec(0)
    "xyLineColor": 2
end setvalues

drw_text("Derivatives from ftcurvpd",0.5,0.95,0.04,xworkid)
drw_text("dx/du",0.65,0.82,0.033,xworkid)
drw_text("dy/du",0.34,0.37,0.033,xworkid)
draw(plotid_function)
frame(xworkid)

;
; End NCL script.
;
end
